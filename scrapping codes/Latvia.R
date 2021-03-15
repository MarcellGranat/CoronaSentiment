# Sun Mar 14 00:54:44 2021 ------------------------------

library(tidyverse)
library(rvest)
library(parallel)

start_date <- seq.Date(from = as.Date(43831, origin = "1899-12-30"), 
                       to = as.Date(44255,origin = "1899-12-30"), by = 1)

URL_df <- as.character(start_date) %>% 
  lapply(function(x) {
    tryCatch({
      read_html(paste0('https://www.lsm.lv/temas/koronaviruss-covid19/?d=', x)) %>% 
        html_nodes('.page-link') %>% 
        html_text() %>% 
        as.numeric() %>%
        na.omit() %>% 
        {tibble(date = x, n_page = .)}},
      error = function(e) tibble(date = x, n_page = 1))
  }) %>% 
  reduce(rbind) %>% 
  mutate(URL = paste0('https://www.lsm.lv/temas/koronaviruss-covid19/?p=', n_page-1, '&d=', date))

f.initial_df <- function(x) {
  tryCatch({
    page <- read_html(x[3])
    data.frame(
      date = x[1],
      title = reduce(html_text(html_nodes(page, '.thumbnail__caption')), c),
      URL = paste0('https://lsm.lv', reduce(html_attr(html_nodes(page, '.thumbnail__caption'), 'href'), c))
    )},
    error = function(e) NULL)
}

f.text <- function(URL) {
  text_a <- tryCatch(URL %>%
                       read_html() %>%
                       html_nodes(".article__lead , .align-left~ p") %>%
                       html_text()
                     ,
                     error = function(e) NA)
  text_b <- tryCatch(URL %>%
                       read_html() %>%
                       html_nodes(".clearfix p") %>%
                       html_text()
                     ,
                     error = function(e) NA)
  str_c(unique(na.omit(c(text_a, text_b))), collapse = T)
}

cl <- makeCluster(7)
clusterExport(cl, list("URL_df", "f.initial_df"), envir = environment())
clusterEvalQ(cl, library(rvest))
clusterEvalQ(cl, library(purrr))

initial_df <- parApply(cl = cl, MARGIN = 1, X = URL_df, FUN = f.initial_df)

initial_df <- reduce(Filter(f = Negate(is.null), initial_df), rbind)

clusterEvalQ(cl, library(stringr))
clusterEvalQ(cl, library(magrittr))
clusterEvalQ(cl, library(dplyr))

clusterExport(cl, list("f.text", "initial_df"), envir = environment())

articles <- parLapply(cl = cl, X = initial_df$URL, fun = f.text)
stopCluster(cl)

Latvia_rawtext <- cbind(initial_df, reduce(articles, c)) %>% 
  tibble() %>% 
  set_names("date", "title", "URL", "text") %>% 
  # filter(text != "") %>% 
  mutate(
    # date = lubridate::dmy(date),
    title = trimws(title), 
    text = trimws(text), 
    text = str_replace_all(text, "\n", " "), # clean text
    text = str_replace_all(text, "\t", " "),
    text = str_replace_all(text, "TRUE", " ")
  )

Latvia_rawtext

setwd("C:/rprojects/CoronaSentiment/scrapping RData")
save(list = c("Latvia_rawtext"), file = "Latvia_rawtext.RData")

