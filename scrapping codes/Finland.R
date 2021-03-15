# Wed Jan 27 23:52:32 2021 ------------------------------

library(tidyverse)
library(rvest)
library(parallel)

pages.raw <- lapply(0:50, function(i) # max 200
  read_html(paste0('https://yle.fi/aihe/t/18-89760?page=', i))
)


URL <- pages.raw %>% 
  lapply(function(page)
    page %>% 
      html_nodes('.w-full>a') %>% 
      html_attr("href") 
  ) %>% reduce(c)

date <- pages.raw %>% 
  lapply(function(page)
    page %>% 
      html_nodes('.pr-11') %>% 
      html_text()
  ) %>% reduce(c)

title <- pages.raw %>% 
  lapply(function(page)
    page %>% 
      html_nodes('.cod\\:txt-20') %>% 
      html_text()
  ) %>% 
  reduce(c)

Finland_rawtext <- data.frame(date, title) %>% 
  mutate(date = lubridate::dmy(date))

read_text <- function(URL) {
  text <- URL %>% 
    read_html() %>% 
    html_nodes(".yle__article__content") %>% 
    html_text() %>% 
    {ifelse(is.null(.), NA, .)}
  
  data.frame(URL = URL, text = text)
}


cl <- makeCluster(7)
clusterExport(cl, list("URL", "read_text"), envir = environment())
clusterEvalQ(cl, library(rvest))
clusterEvalQ(cl, library(magrittr))

articles <- parLapply(cl = cl, X = URL, fun = read_text)

stopCluster(cl)

Finland_rawtext <- cbind(Finland_rawtext, reduce(articles, rbind))

# saving --------------------------------------------------------
setwd("C:/rprojects/CoronaSentiment/scrapping RData")
save(list = c("Finland_rawtext"), file = "Finland_rawtext2.RData")