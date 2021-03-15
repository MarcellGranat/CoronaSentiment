# Mon Mar 01 22:14:58 2021 ------------------------------

library(tidyverse)
library(rvest)
library(parallel)

URLs <- paste0('https://www.diepresse.com/suche?s=coronavirus&p=', 1:461)

f.initial_df <- function(URL) {
  tryCatch({
    page <- read_html(URL)
    data.frame(
      title = reduce(html_text(html_nodes(page, '.card__link')), c),
      URL = reduce(html_attr(html_nodes(page, '.card__link'), 'href'), c)
    )},
    error = function(e) NULL)
}

f.text <- function(URL) {
  tryCatch({article_page <- URL %>%
             read_html()
           
data.frame(URL, date = article_page %>% 
    html_nodes(".meta__date") %>% 
    html_text() %>% 
    str_c(collapse = " "),
         text = article_page %>% 
             html_nodes("#article-body p") %>%
             html_text() %>%
             str_c(collapse = " "))}
           ,
           error = function(e) NA)
}

cl <- makeCluster(7)
clusterExport(cl, list("URLs", "f.initial_df"), envir = environment())
clusterEvalQ(cl, library(rvest))
clusterEvalQ(cl, library(purrr))

initial_df <- parLapply(cl = cl, X = URLs, fun = f.initial_df)
initial_df <- reduce(Filter(f = Negate(is.null), initial_df), rbind)

clusterEvalQ(cl, library(magrittr))
clusterEvalQ(cl, library(stringr))
clusterExport(cl, list("f.text", "initial_df"), envir = environment())

articles <- parLapply(cl = cl, X = initial_df$URL, fun = f.text)
stopCluster(cl)

Austria_rawtext <- merge(initial_df, reduce(articles, rbind)) %>% 
  select(date, title, URL, text) %>% 
  filter(text != "") %>% 
  mutate(
    date = str_remove_all(date, "\n"),
    date = gsub(" um.*", "", date),
    date = lubridate::dmy(date),
    title = gsub(".*Premium", "", title),
    title = str_remove_all(title, '  '),
    title = str_remove_all(title, "\n"),
    title = str_remove_all(title, "\t"),
    text = str_remove_all(text, '  '),
    text = str_replace_all(text, "\n", " "), # clean text
    text = str_replace_all(text, "\t", " "),
    text = str_replace_all(text, '"', " "),
  )

setwd("C:/rprojects/CoronaSentiment/scrapping RData")
save(list = c("Austria_rawtext"), file = "Austria_rawtext.RData")

