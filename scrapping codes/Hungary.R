library(tidyverse)
library(magrittr)

for (i in 1:546){
  URL <- paste("https://hirado.hu/wp-content/plugins/hirado.hu.widgets/widgets/newSearch/ajax_loadmore.php?s=koronav%C3%ADrus&media_type=article&page_number=", i, "&gallery_slug=galeriak&video_slug=videok&video_category=431&elasticPage=-1&elasticPostPage=-1&allVideos=false&galleriesNum=0&videosNum=0", sep="")
  list <- append(list, data_scrape_hun(i = i, link = URL))
}


for (i in 1:10320){
  if (typeof(newlist[[i]]) == "integer"){
    URL <- paste("https://hirado.hu/wp-content/plugins/hirado.hu.widgets/widgets/newSearch/ajax_loadmore.php?s=koronav%C3%ADrus&media_type=article&page_number=", newlist[[i]], "&gallery_slug=galeriak&video_slug=videok&video_category=431&elasticPage=-1&elasticPostPage=-1&allVideos=false&galleriesNum=0&videosNum=0", sep="")
    newlist <- append(newlist, data_scrape_hun(i = newlist[[i]], link = URL))
  }
}

list <- append(list, newlist)

df <- data.frame(links = matrix(unlist(list), nrow=10941, byrow=TRUE), stringsAsFactors=FALSE)
library(dplyr)
df_2 <- df %>% 
  filter(nchar(links)>5) %>% 
  mutate(links = gsub("^//hirado", "https://hirado", links))


data_scrape_hun <- function(i, link){
  tryCatch(
    expr = {
      print(i)
      page <- read_html(URL)
      return(c(paste(html_text(page %>% rvest::html_nodes(".articleContent>p, .articleLead>strong>p")), collapse = " "), 
               html_text(page %>% rvest::html_nodes(".artTime")), html_text(page %>% rvest::html_nodes("h1"))))
    },
    error = function(e){
      if (grepl("404", e, fixed = TRUE)){
        timer(0)
      }
      else{
        message('Caught an error!')
        print(e)
        timer(2)
      }
      
    },
    warning = function(w){
      message('Caught a warning!')
      print(w)
    }
  )    
}

for (i in 1:nrow(df_2)){
  URL <- df_2$links[i]
  dataframe <- data_scrape_hun(i = i, link = URL)
  df_2$date[i] <- dataframe[2]
  df_2$text[i] <- dataframe[1]
  df_2$title[i] <- dataframe[3]
}

df_2 %<>% 
  select(date, title, URL = links, text) %>% 
  #mutate_at(-1, function(x) zoo::na.locf(x)) %>% 
  #filter(!str_detect(date, '_x000') & !str_detect(date, ':') & date != '0') %>% 
  #filter(!str_detect(text, 'mtva_player')) %>% # TODO consider a better solution
  mutate(
    date = gsub("\\s+", " ", str_trim(date)),
    title = gsub("\\s+", " ", str_trim(title)),
    text = gsub("\\s+", " ", str_trim(text))
  )


write.csv(df_2, "Hungary_rawtext.csv")