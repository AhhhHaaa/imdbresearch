library(rvest)
library(tidyverse)
library(stringr)

url <- paste0("https://www.imdb.com/search/title/", 
       "?title_type=tv_series&release_date=",
       "2005-01-01,2019-10-31&countries=gb")
imdb <- read_html(url)

ranking <- imdb %>% 
    html_nodes(".text-primary") %>% 
    html_text() %>% 
    as.numeric() #change into numeric format

title <- imdb %>% 
    html_nodes(".lister-item-header a") %>% 
    html_text()

rating <- imdb %>% 
  html_nodes(".ratings-imdb-rating strong") %>% 
  html_text() 
  # as.numeric() #cannot be changed into numeric

time <- imdb %>% 
  html_node(html_nodes(imdb , ".runtime"), ".runtime") %>% 
  html_text() %>% 
  #as.numeric(time) %>%  #change into numeric format
str_sub(1,2)

description <- imdb %>% 
  html_nodes(".ratings-bar+ .text-muted") %>% 
  html_text()
description <- gsub("\n","", description) %>%  #gsub() to remove \n
#str_trim(string, side = c("both", "left", "right"))
#remove the blanks 
str_trim( side = "left") #remove the left blanks


genre <- imdb %>% 
  html_nodes(".genre") %>% 
  html_text() 
  genre <- gsub("\n", "", genre) %>%  str_trim("right")
  genre <- gsub(",.*", "", genre) 
  genre <- as.factor(genre)
  
vote <- imdb %>% 
  html_nodes(".text-muted+ span") %>% 
  html_text()
vote <- gsub(",", "",vote)

stars <- imdb %>% 
  html_nodes(".lister-item-content a:nth-child(1)") %>% 
  html_text() %>% 
  as.factor()



imdb_tv <- data.frame(
  Ranking = ranking,
  Title = title,
  Rating = rating,
  Genre = genre,
  Description = description,
  #Time = time, leave them alone, waiting for sort out NAs
  #Stars = stars,  leave them alone, waiting for sort out NAs
  Vote = vote) #arguments imply differing number of rows 

test_length <- list(ranking, title, rating, 
                    genre, description, 
                    time, stars, vote)# making a list to futher test
sapply(test_length, length) # apply lenth to each variables




