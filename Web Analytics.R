#--------------------- Web Analytics ----------------------

# Install Selector Gadget for Goggle Chrome by using the link below
# https://chrome.google.com/webstore/detail/selectorgadget/mhjhnkcfbdhnjickkkdbjoemdmbfginb
# Click on Add to chrome

# install.packages("rvest") 

library(rvest)

# Now, let's get started with scraping the IMDb website for 
# the 100 most popular feature films released in 2016

# Specifying the url for desired website to be scrapped

url <- 'http://www.imdb.com/search/title?count=100&release_date=2016,2016&title_type=feature'

#Reading the HTML code from the website
webpage <- read_html(url)
webpage

# we'll be scraping the following data from this website.
# .	Rank: The rank of the film from 1 to 100 
# .	Title: The title of the feature film.
# .	Description: The description of the feature film.
# .	Runtime: The duration of the feature film.
# .	Genre: The genre of the feature film,
# .	Rating: The IMDb rating of the feature film.
# .	Metascore: The metascore on IMDb website for the feature film.
# .	Votes: Votes cast in favor of the feature film.
# .	Gross_Earning_in_Mil: The gross earnings of the feature film in millions.
# .	Director: The main director of the feature film. Note, in case of multiple directors, I'll take only the first.
# .	Actor: The main actor of the feature film. Note, in case of multiple actors, I'll take only the first.

# Using CSS selectors to scrap the rankings section
rank_data_html <- html_nodes(webpage,'.text-primary')

#Converting the ranking data to text
rank_data <- html_text(rank_data_html)

#Let's have a look at the rankings
head(rank_data)

#Data-Preprocessing: Converting rankings to numerical
rank_data<-as.numeric(rank_data)

#Let's have another look at the rankings
head(rank_data)
length(rank_data)

#------------------------------------------------------------------------
#Using CSS selectors to scrap the title section
title_data_html <- html_nodes(webpage,'.lister-item-header a')

#Converting the title data to text
title_data <- html_text(title_data_html)

#Let's have a look at the title
head(title_data)

#------------------------------------------------------------------------
#Using CSS selectors to scrap the description section
description_data_html <- html_nodes(webpage,'.ratings-bar+ .text-muted')

#Converting the description data to text
description_data <- html_text(description_data_html)

#Let's have a look at the description data
head(description_data,1)

#Data-Preprocessing: removing '\n'
description_data<-gsub("\n","",description_data)

#Let's have another look at the description data 
head(description_data,1)
length(description_data)
#------------------------------------------------------------------------
#Using CSS selectors to scrap the Movie runtime section
runtime_data_html <- html_nodes(webpage,'.text-muted .runtime')

#Converting the runtime data to text
runtime_data <- html_text(runtime_data_html)

#Let's have a look at the runtime
head(runtime_data)

#Data-Preprocessing: removing mins and converting it to numerical

runtime_data<-gsub(" min","",runtime_data)
runtime_data<-as.numeric(runtime_data)

#Let's have another look at the runtime data
head(runtime_data)
length(runtime_data)

#------------------------------------------------------------------------
#Using CSS selectors to scrap the Movie genre section
genre_data_html <- html_nodes(webpage,'.genre')

#Converting the genre data to text
genre_data <- html_text(genre_data_html)

#Let's have a look at the runtime
head(genre_data)

#Data-Preprocessing: removing \n
genre_data<-gsub("\n","",genre_data)

#Data-Preprocessing: removing excess spaces
genre_data<-gsub(" ","",genre_data)

#taking only the first genre of each movie
genre_data<-gsub(",.*","",genre_data)

#Convering each genre from text to factor
genre_data<-as.factor(genre_data)

#Let's have another look at the genre data
head(genre_data)
length(genre_data)
#------------------------------------------------------------------------

#Using CSS selectors to scrap the IMDB rating section
rating_data_html <- html_nodes(webpage,'.ratings-imdb-rating strong')

#Converting the ratings data to text
rating_data <- html_text(rating_data_html)

#Let's have a look at the ratings
head(rating_data)
#Data-Preprocessing: converting ratings to numerical
rating_data<-as.numeric(rating_data)

#Let's have another look at the ratings data
head(rating_data)
length(rating_data)
#------------------------------------------------------------------------

#Using CSS selectors to scrap the votes section
votes_data_html <- html_nodes(webpage,'.sort-num_votes-visible span:nth-child(2)')

#Converting the votes data to text
votes_data <- html_text(votes_data_html)

#Let's have a look at the votes data
head(votes_data)
#Data-Preprocessing: removing commas
votes_data<-gsub(",","",votes_data)

#Data-Preprocessing: converting votes to numerical
votes_data<-as.numeric(votes_data)

#Let's have another look at the votes data
head(votes_data)
length(votes_data)
#------------------------------------------------------------------------

#Using CSS selectors to scrap the directors section
directors_data_html <- html_nodes(webpage,'.text-muted+ p a:nth-child(1)')

#Converting the directors data to text
directors_data <- html_text(directors_data_html)

#Let's have a look at the directors data
head(directors_data)

#Data-Preprocessing: converting directors data into factors
directors_data<-as.factor(directors_data)

#Using CSS selectors to scrap the actors section
actors_data_html <- html_nodes(webpage,'.lister-item-content .ghost+ a')

#Converting the gross actors data to text
actors_data <- html_text(actors_data_html)

#Let's have a look at the actors data
head(actors_data)

#Data-Preprocessing: converting actors data into factors
actors_data<-as.factor(actors_data)
length(actors_data)
#------------------------------------------------------------------------

#Using CSS selectors to scrap the metascore section
metascore_data_html <- html_nodes(webpage,'.metascore')

#Converting the runtime data to text
metascore_data <- html_text(metascore_data_html)

#Let's have a look at the metascore data
head(metascore_data,6)

#Data-Preprocessing: removing extra space in metascore
metascore_data<-gsub(" ","",metascore_data)

#Lets check the length of metascore data
length(metascore_data)

#The length of meta score data is 96 while we are scrapping the data for 100 movies. 
# The reason this happened is because there are 4 movies which don't have the 
# corresponding Metascore fields.

metascore_data <- append(list("NA"),metascore_data)
length(metascore_data)

for (i in c(44, 59,63)){
  
  a<-metascore_data[1:(i-1)]
  
  b<-metascore_data[i:length(metascore_data)]
  
  metascore_data<-append(a,list("NA"))
  
  metascore_data<-append(metascore_data,b)
  
}

#Data-Preprocessing: converting metascore to numerical
metascore_data<-as.numeric(metascore_data)

#Let's have another look at length of the metascore data

length(metascore_data)

#Let's look at summary statistics
summary(metascore_data)
#------------------------------------------------------------------------

#Combining all the lists to form a data frame
movies_df<-data.frame(Rank = rank_data, Title = title_data,
                      
                      Description = description_data, Runtime = runtime_data,
                      
                      Genre = genre_data, Rating = rating_data,
                      
                      Metascore = metascore_data, Votes = votes_data,
                      
                      Director = directors_data, Actor = actors_data)

#Structure of the data frame

str(movies_df)
View(movies_df)
# You have now successfully scrapped the IMDb website for 
# the 100 most popular feature films released 

#-------------------------------------------------------------------------
# Analyzing scrapped data from the web

library('ggplot2')

qplot(data = movies_df,Runtime,fill = Genre,bins = 30)
#Based on the above data, which movie from which Genre had the longest runtime?

ggplot(movies_df,aes(x=Runtime,y=Rating))+
  geom_point(aes(size=Votes,col=Genre))
#Based on the above data, in the Runtime of 130-160 mins, which genre has the highest votes?
