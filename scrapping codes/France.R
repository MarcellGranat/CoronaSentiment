# Thu Jan 28 20:39:14 2021 ------------------------------

library(tidyverse)
library(rvest)

URLs<- c('https://www.france24.com/fr/tag/covid-19/#paper',
             paste0('https://www.france24.com/fr/tag/covid-19/', 2:218, '/#paper')
)

f.URL <- function(page) {
    page %>%
      html_nodes('.m-item-list-article >a') %>%
      html_attr("href") %>%
      na.omit %>%
      {str_c("https://www.france24.com", .)} %>%
  reduce(c)
}

f.date <- function(page) {
    page %>%
      html_nodes('time') %>%
      html_text()  %>%
  lubridate::dmy() %>%
  reduce(c)
}

f.title <- function(page) {
    page %>%
      html_nodes('.article__title') %>%
      html_text() %>%
  reduce(c)
}

f.text <- function(URL) {
  tryCatch(URL %>%
    read_html() %>%
    html_nodes(".u-clearfix p") %>%
    html_text %>%
    str_c(collapse = " ") %>%
    {gsub('\\"', " ", .)} %>%
    {gsub('>>"', "", .)} %>%
    {gsub('>>', "", .)} %>%
    {gsub('Avec AFP.*', "", .)} %>% # remove add texts
    {gsub('Téléchargez l\'application France 24', "", .)} %>%
    {gsub('Retrouvez tous les matins sur France 24 la Revue de presse', "", .)} %>%
    {gsub('du lundi au vendredi', "", .)} %>%
    {gsub('7h20 et 9h20 heure de Paris', "", .)} %>%
    {gsub('Suivez également tous les week-ends en multidiffusion la Revue des Hebdos.', "", .)} %>%
    {gsub('Le résumé de la semaineFrance 24 vous propose de revenir sur les actualités qui ont marqué la semaine Emportez l\'actualité internationale partout avec vous !', "", .)},
  error = function(e) NA)
}
# test ----------------------------------------------------------

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
France_rawtext <- text_df %>% 
  filter(!duplicated(text)) # remove duplicates

setwd("C:/rprojects/CoronaSentiment/scrapping RData")
save(list = c("France_rawtext"), file = "France_rawtext.RData")
