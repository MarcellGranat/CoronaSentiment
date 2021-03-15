# Fri Jan 29 01:38:25 2021 ------------------------------

library(tidyverse)
library(rvest)

URLs <- paste0('https://bntnews.bg/covid19.html?page=', 1:148)

f.URL <- function(page) {
  page %>%
    html_nodes('.img-wrap a, .img-wrap1') %>%
    html_attr("href") %>%
    reduce(c)
}

f.date <- function(page) {
  page %>%
    html_nodes('.news-time') %>%
    html_text()  %>%
    {gsub(".*,", "", .)} %>% 
    {gsub("\n.*", "", .)} %>% 
    lubridate::dmy() %>% 
    reduce(c)
}

f.title <- function(page) {
  page %>%
    html_nodes('.img-title') %>%
    html_text() %>%
    str_remove_all("\n") %>% 
    str_remove_all("  ") %>% 
    reduce(c)
}

f.text <- function(URL) {
  tryCatch(URL %>%
             read_html() %>%
             html_nodes("p") %>%
             html_text %>%
             str_replace_all('["."]', " ") %>% 
             {gsub("\n", " ", .)} %>% 
             str_c(collapse = " ") %>% 
             str_remove_all("  "),
           error = function(e) NA)
}

f.rawtext <- function(raw.page) {
  tryCatch(
    {page <- read_html(raw.page)
    URL <- f.URL(page)
    tibble(date = f.date(page), title = f.title(page), URL = URL, text = sapply(URL, f.text))},
    error = function(e) {print(paste("Error with", raw.page)); NULL})
}

f.new_rawtext <- function(URLs) {
  lapply(URLs, f.rawtext) %>%
    Filter(f = Negate(is.null)) %>%
    reduce(rbind) %>%
    filter(text != "")
}

# test ----------------------------------------------------------
read_html(URLs[1]) %>% 
  f.date

read_html(URLs[1]) %>% 
  f.title()

read_html(URLs[1]) %>% 
  f.URL()

read_html(URLs[1]) %>% 
  f.URL %>% 
  .[[1]] %>% 
  f.text()

f.new_rawtext(URLs[1:2])

# start scrapping -----------------------------------------------

i <- 1
cat(paste0((i-1)*10, "% process"))

f.get_text <- function() {
  quantile(seq_along(URLs), probs = 0:10/10) %>% 
    floor() %>% 
    .[i:(i+1)] %>% 
    {i <<- i + 1; URLs[.[1]:.[2]]} %>% 
    f.new_rawtext
}

text_df <- f.get_text()
cat(paste0((i-1)*10, "% process"))

text_df <- rbind(text_df, f.get_text())
cat(paste0((i-1)*10, "% process"))

text_df <- rbind(text_df, f.get_text())
cat(paste0((i-1)*10, "% process"))

text_df <- rbind(text_df, f.get_text())
cat(paste0((i-1)*10, "% process"))

text_df <- rbind(text_df, f.get_text())
cat(paste0((i-1)*10, "% process"))

text_df <- rbind(text_df, f.get_text())
cat(paste0((i-1)*10, "% process"))

text_df <- rbind(text_df, f.get_text())
cat(paste0((i-1)*10, "% process"))

text_df <- rbind(text_df, f.get_text())
cat(paste0((i-1)*10, "% process"))

text_df <- rbind(text_df, f.get_text())
cat(paste0((i-1)*10, "% process"))

text_df <- rbind(text_df, f.get_text())
cat(paste0((i-1)*10, "% process"))


# saving --------------------------------------------------------
Bulgaria_rawtext <- text_df %>% 
  filter(!duplicated(text)) # remove duplicates

setwd("C:/rprojects/CoronaSentiment/scrapping RData")
save(list = c("Bulgaria_rawtext"), file = "Bulgaria_rawtext.RData")
