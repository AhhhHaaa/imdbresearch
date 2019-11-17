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
  genre <- gsub("\n", "", genre)
  genre <- word(genre, 1, sep = ',') %>%  str_trim("right")
  
  
vote <- imdb %>% 
  html_nodes(".text-muted+ span") %>% 
  html_text()
vote <- gsub(",", "",vote) %>% as.numeric()

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


##TOP30 TV series in the UK
top30 <- imdb_tv %>% 
  filter(Ranking <= 30) %>% 
  ggplot(aes(Rating, Ranking)) +
  geom_point(aes(colour = Genre)) +
  coord_flip()

##TOP 10 voted TV series in the UK

Top10voted <- imdb_tv %>% 
  group_by(Vote, Ranking) %>% 
  filter(Vote > 10000) %>% top_n(n = 30) %>%  #ton_n() to find the top30
  ggplot(aes(Vote, Ranking)) +
  geom_point(aes(color = Genre))

#try to plot the Parallel coordinates

