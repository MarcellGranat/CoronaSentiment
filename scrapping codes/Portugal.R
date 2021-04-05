library(jsonlite)
library(tidyverse)
library(rvest)
library(parallel)


URLs <- paste0("https://www.rtp.pt/noticias/rc/playlist/topics/1293026/covid-19/", 1:2843)

f.json <- function(URL) {
  tryCatch(fromJSON(URL)$url, error = function(e) c(NA))
}

f.gettext <- function(URL) {
  tryCatch({
    page <- read_html(URL)
    df <- data.frame(
      date = page %>% html_node('.artigo-data>.text-gray>time') %>% html_attr("pub-data") %>% paste(collapse = " "),
      title = page %>% html_nodes("h1") %>% html_text() %>% paste(collapse = " "),
      URL = URL,
      text = page %>% 
        html_nodes('.article-lead , .article-body p') %>% 
        html_text() %>% 
        paste(collapse = " ")
    )
    if (df[1, 1] == "NA") {
      df[1, 1] = page %>% html_nodes(".artigo-data>.text-gray>time") %>% html_attr("title") %>% paste(collapse = " ")
    } 
    df
  }
  ,
  error = function(e) NULL)
}

cl <- makeCluster(7)
clusterExport(cl, list("URLs", "f.json"), envir = environment())
clusterEvalQ(cl, library(rvest))
clusterEvalQ(cl, library(jsonlite))
clusterEvalQ(cl, library(purrr))
clusterEvalQ(cl, library(magrittr))
clusterEvalQ(cl, library(stringr))

URLsToArticle <- parLapply(cl = cl, X = URLs, fun = f.json)

URLsToArticle <- reduce(URLsToArticle, c)

URLsToArticle <- na.omit(URLsToArticle)

clusterExport(cl, list("f.gettext", "URLsToArticle"), envir = environment())
print('scrape!')

Portugal_rawtext <- parLapply(cl = cl, X = URLsToArticle, fun = f.gettext)
stopCluster(cl)

Portugal_rawtext <- reduce(Filter(f = Negate(is.null), Portugal_rawtext), rbind) %>% 
  mutate_all(function(x) {
    str_remove_all(x, "\r") %>% 
      str_remove_all("\n") %>% 
      str_remove_all("\t") %>% 
      str_remove_all("  ")
  } 
  ) %>% mutate(
    date = str_remove_all(date, "publicado "),
    date = gsub(",.*", "", date)
  )

setwd("C:/rprojects/CoronaSentiment/scrapping RData")
save(list = c("Portugal_rawtext"), file = "Portugal_rawtext.RData")
