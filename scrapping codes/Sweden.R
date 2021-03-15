library(jsonlite)
library(tidyverse)
library(rvest)
library(parallel)


URLs <- paste0("https://api.svt.se/nss-api/page/nyheter/utrikes/25393539?q=articles%2Climit%3D10%2Cpage%3D", 1:648)

f.json <- function(URL) {
  fromJSON(URL)[["articles"]][["content"]][["articles"]][[1]][["articles"]][[1]][["teaserURL"]] %>%
    {ifelse(str_detect(., 'https://www'),.,  paste0("http://www.svt.se", .))}
}

f.gettext <- function(URL) {
  tryCatch({
    page <- read_html(URL)
    data.frame(
      date = page %>% html_nodes(".nyh_article__date-timestamp") %>% html_text() %>% paste(collapse = " "),
      title = page %>% html_nodes(".nyh_article__heading") %>% html_text() %>% paste(collapse = " "),
      URL = URL,
      text = page %>% 
        html_nodes('.nyh_article__lead p , .lp_body p') %>% 
        html_text() %>% 
        paste(collapse = " ")
    ) }
    ,error = function(e) NULL)
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

URLsToArticle <- URLsToArticle %>% 
  {ifelse(str_detect(., "/datajournalistik/"), NA, .)} %>% 
  na.omit()

clusterExport(cl, list("f.gettext", "URLsToArticle"), envir = environment())

Sweden_rawtext <- parLapply(cl = cl, X = URLsToArticle, fun = f.gettext)
stopCluster(cl)

Sweden_rawtext <- reduce(Filter(f = Negate(is.null), Sweden_rawtext), rbind) %>% 
  tibble()

setwd("C:/rprojects/CoronaSentiment/scrapping RData")
save(list = c("Sweden_rawtext"), file = "Sweden_rawtext.RData")