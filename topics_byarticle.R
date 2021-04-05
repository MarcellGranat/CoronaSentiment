message('Start!')

library(tidyverse)
library(knitr)
library(tidytext)
library(tm)
library(lubridate)
library(topicmodels)


load("C:/rprojects/CoronaSentiment/dat_modified_bing.RData")
rm(list = setdiff(ls(), 'dat'))

load('C:/rprojects/CoronaSentiment/topic_models/mod12.RData')

v <- seq(from = 1, to = nrow(dat), length.out = 1000) %/% 1

dat_topics <- tibble(
  date = date(),
  country = character(),
  topic_1 = numeric(),
  topic_2 = numeric(),
  topic_3 = numeric(),
  topic_4 = numeric(),
  topic_5 = numeric(),
  topic_6 = numeric(),
  topic_7 = numeric(),
  topic_8 = numeric(),
  topic_9 = numeric(),
  topic_10 = numeric(),
  topic_11 = numeric(),
  topic_12 = numeric()
)

for (i in 1:(length(v) - 1)) {
  message(i)
  dat_topics <- dat[v[i]:v[i + 1], ] %>%
    filter(str_remove_all(text, '\\W') != '') %>% 
    mutate(id = row_number()) %>% 
    unnest_tokens('words', 'text') %>% 
    count(id, words, sort = T) %>% 
    cast_dfm(id, words, n) %>% 
    {posterior(mod, .)} %>% 
    .$topics %>% 
    data.frame() %>% 
    {cbind(select(filter(dat[v[i]:v[i + 1], ], str_remove_all(text, '\\W') != ''),
           date, country), .)} %>% 
    rename_all(.funs = function(x) str_replace_all(x, 'X', 'topic_')) %>% 
    {rbind(dat_topics, .)}
}

save(list = c('dat_topics'), file = 'C:/rprojects/CoronaSentiment/topics_bydat.RData')
message('done!')

tcltk::tkmessageBox(title = "Job done!",
                    message = paste('Job done at', Sys.time()), icon = "info", type = "ok")
