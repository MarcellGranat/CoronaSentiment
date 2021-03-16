# Fri Jan 29 02:08:41 2021 ------------------------------

library(tidyverse)
library(rvest)
library(parallel)

URLs <- c(paste0('https://nos.nl/zoeken/?q=covid&page=', 0:56),
          paste0('https://nos.nl/zoeken/?q=coronavirus&page=', 0:214))
%>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% <- 

f.initial_df <- function(URL) {
  tryCatch({
    page <- read_html(URL)
    data.frame(
      date = reduce(html_text(
        html_nodes(page, '.search-results__time')), c),
      title = reduce(html_text(
        html_nodes(page, '.search-results__title')), c),
      URL = paste0("https://www.nos.nl", 
                   reduce(html_attr(
                     html_nodes(page,'.search-results__item>a'), 'href'), c))
    )},
    error = function(e) NULL)
}

f.text <- function(URL) { # TODO
  if (str_detect(URL, "nos.nl/video", T)) { 
    # if the link points to a video, then NA
    tryCatch(URL %>%
               read_html() %>%
               html_nodes(".text_3v_J6Y0G") %>%
               html_text %>% 
               str_remove_all("\n") %>% 
               str_c(collapse = " "),
             error = function(e) NA)
  } else {
    NA
  }
}


cl <- makeCluster(1)
clusterExport(cl, list("URLs", "f.initial_df"), envir = environment())
clusterEvalQ(cl, library(rvest))
clusterEvalQ(cl, library(purrr))
clusterEvalQ(cl, library(magrittr))

initial_df <- parLapply(cl = cl, X = URLs, fun = f.initial_df)
initial_df <- reduce(Filter(f = Negate(is.null), initial_df), rbind) %>% 
  dplyr::filter(!duplicated(URL))

clusterEvalQ(cl, library(stringr))
clusterExport(cl, list("f.text", "initial_df"), envir = environment())

articles <- parLapply(cl = cl, X = initial_df$URL, fun = f.text)
stopCluster(cl)

Netherlands_rawtext <- cbind(initial_df, reduce(articles, c)) %>% 
  set_names("date", "title", "URL", "text") %>% 
  filter(text != "") # TODO manage date

save(list = c("Netherlands_rawtext"), file = "C:/rprojects/CoronaSentiment/scrapping RData/Netherlands_rawtext.RData")
%>% 