library(jsonlite)
library(tidyverse)
library(rvest)
library(parallel)


URLs <- paste0("https://www.err.ee/api/search/getContentByKeyword/?options=%7B%22total%22:0,%22page%22:1,%22limit%22:20,%22offset%22:",seq(from = 0, to =4440, by = 20),",%22phrase%22:%22%22,%22publicStart%22:%22%22,%22publicEnd%22:%22%22,%22keywordId%22:2216372%7D")


f.json <- function(URL) {
  tryCatch(fromJSON(URL)[["contents"]][["url"]], error = function(e) c(NA))
}

f.gettext <- function(URL) {
  tryCatch({
    page <- read_html(URL)
    data.frame(
      date = page %>% html_nodes('.pubdate') %>% html_attr('datetime') %>% paste(collapse = " "),
      title = page %>% html_nodes(".no-border h1") %>% html_text() %>% paste(collapse = " "),
      URL = URL,
      text = page %>% 
        html_nodes('.body div p') %>% 
        html_text() %>% 
        paste(collapse = " ")
    ) }
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

Estonia_rawtext <- parLapply(cl = cl, X = URLsToArticle, fun = f.gettext)
stopCluster(cl)

Estonia_rawtext <- reduce(Filter(f = Negate(is.null), Estonia_rawtext), rbind) %>% 
  mutate_all(function(x) {
    str_remove_all(x, "\r") %>% 
      str_remove_all("\n") %>% 
      str_remove_all("\t") %>% 
      str_remove_all("  ")
  } 
  ) %>% 
  mutate(date = gsub("T.*", "", date))

for (i in 2:nrow(Estonia_rawtext)) { # imputing missing date values
  if (Estonia_rawtext$date[i] != "NA") {
    last_date <- Estonia_rawtext$date[i]
  } else {
    Estonia_rawtext[i, 1] <- Estonia_rawtext %>% # if the following known date equals to the previous known one,
      tail(-i) %>%                               # then input, in other case leave it
      filter(date != "NA") %>% 
      .[1, ] %>% 
      pull(date) %>% 
      {ifelse(. == last_date, last_date, "NA")}
  }
}

Estonia_rawtext <- Estonia_rawtext %>% 
  filter(date != "NA")

setwd("C:/rprojects/CoronaSentiment/scrapping RData")
save(list = c("Estonia_rawtext"), file = "Estonia_rawtext.RData")


