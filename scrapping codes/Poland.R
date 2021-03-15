library(tidyverse)
library(rvest)
library(parallel)

initial_df <- readxl::read_excel("c:/rprojects/CoronaSentiment/scrapping RData/poland_content.xlsx") %>% 
  mutate(
    date = lubridate::ymd(date)
  ) %>% 
  filter(!str_detect(link, 'wiadomosci.tvp.pl'))
# Poland_rawtext <- apply(initial_df, 1, FUN = function(x) {
#   tryCatch({
#   page <- read_html(x[3])
#   
#   tobesubtracted <- page %>% 
#     rvest::html_nodes(".am-article__image, .facebook-paragraph, .am-article__source, .article-tags, .article-layout>script, .article-layout>style") %>%
#     html_text() %>% 
#     str_replace_all("[()@.{}+:;]", " ") %>% 
#     str_replace_all("\n", " ")
#   text <- page %>% 
#     html_nodes(".layout-article>.article-layout") %>%
#     html_text()
#   if (str_length(paste0(tobesubtracted, collapse = ' ')) > 0) {
#   text <- text %>%
#     str_replace_all("[()@.{}+:;]", " ") %>% 
#     str_replace_all("\n", " ") %>% 
#     str_remove_all(paste(tobesubtracted, collapse = "|")) %>% 
#     str_replace_all("  *", " ")
#   }
#   
#   data.frame(date = x[2], title = x[1], URL = x[3], text = paste0(text, collapse = ' '))
#   }, error = function(e) NULL)
# })

Poland_rawtext <- data.frame(date = "aa", title = "aa", URL = "aa", text = "aa")
initial_df <- initial_df %>% 
  rename(URL = link)


for (i in 1:nrow(initial_df)) {
  tryCatch({
  page <- read_html(pull(initial_df, 3)[i])
  tobesubtracted <- page %>% 
    html_nodes(".am-article__image, .facebook-paragraph, .am-article__source, .article-tags, .article-layout>script, .article-layout>style") %>%
    html_text() %>% 
    str_replace_all("[()@.{}+:;]", " ") %>% 
    str_replace_all("\n", " ")
  text <- page %>% 
    html_nodes(".layout-article>.article-layout") %>%
    html_text()
  if (str_length(paste0(tobesubtracted, collapse = ' ')) > 0) {
    text <- text %>%
      str_replace_all("[()@.{}+:;]", " ") %>% 
      str_replace_all("\n", " ") %>% 
      str_remove_all(paste(tobesubtracted, collapse = "|")) %>% 
      str_replace_all("  *", " ")
  }
  
  })
  Poland_rawtext <- rbind(Poland_rawtext,
                           data.frame(date = initial_df[i, 2],
                                      title = initial_df[i, 1],
                                      URL = initial_df[i, 3],
                                      text = paste0(text, collapse = ' ')))
  cat(paste0(i, ", "))
}



setwd("C:/rprojects/CoronaSentiment/scrapping RData")
save(list = c("Poland_rawtext"), file = "Poland_rawtext.RData")
