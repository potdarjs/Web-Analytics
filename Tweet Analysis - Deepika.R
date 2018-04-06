

# Tweet Analysis----------------------------------

getwd()
setwd("D:\\Data Analytics with RET\\Web Analytics")
# install.packages("twitteR", dependencies = TRUE, repos = "http://cran.rstudio.com")
# install.packages("ROAuth")
# install_github ("geoffjentry/twitteR") # install 'twitteR'
 
library("twitteR")
library("ROAuth")
library(httr)
library(devtools)



# Download the file and store in your working directory
download.file(url= "http://curl.haxx.se/ca/cacert.pem", destfile= "cacert.pem")

#Insert your consumerKey and consumerSecret below
credentials <- OAuthFactory$new(consumerKey='60fuybqQUae64cCQKFhx6KlOk',
                                consumerSecret='	hdWfquzLLKjeCFDLttivm4hUovgmUaVQOD4wY3m9TQ46880gcN',
                                requestURL='https://api.twitter.com/oauth/request_token',
                                accessURL='https://api.twitter.com/oauth/access_token',
                                authURL='https://api.twitter.com/oauth/authorize')

credentials$handshake(cainfo="cacert.pem")

# Save the credentials for later use

save(credentials, file="twitter authentication.Rdata")


setup_twitter_oauth ('60fuybqQUae64cCQKFhx6KlOk', 
                     'hdWfquzLLKjeCFDLttivm4hUovgmUaVQOD4wY3m9TQ46880gcN', 
                     '1700685204-ODv53QCI5nCfar54YfpmEflGFQY5bThYdoGPfFO', 
                     'EnRINbRyb1d25LwB2S8qjCPQBT4mU00ebWIoRVsnSECb3')  # authenticate


#Extract Tweets
#To get tweets from a particular user

#   Tweets by Deepika Padukone---------------
tweetsby <- userTimeline('deepikapadukone',n=1000) # tweets from a user
head(tweetsby)

#-------------------------------------------------------------------------------
# Tweets containing Deepika Padukone-------------
tweetson <- searchTwitter("deepikapadukone", n=25)
tweetson
# Analysis of Tweets on Deepika Padukone is done at row no.
#-------------------------------------------------------------------------------

tweetsby <- twListToDF(tweetsby)

# Lets further clean up these tweets. 
# The below code will process your tweets for punctuations, URLs, hashtags, mentions etc. 

TextPreprocessing <- lapply(unlist(tweetsby$text), function(x) {
  
  x = gsub('http\\S+\\s*', '', x) ## Remove URLs
    x = gsub('\\b+RT', '', x) ## Remove RT
    x = gsub('#\\S+', '', x) ## Remove Hashtags
    x = gsub('@\\S+', '', x) ## Remove Mentions
    x = gsub('[[:cntrl:]]', '', x) ## Remove Controls and special characters
    x = gsub("\\d", '', x) ## Remove Controls and special characters
    x = gsub('[[:punct:]]', '', x) ## Remove Punctuations
    x = gsub("^[[:space:]]*","",x) ## Remove leading whitespaces
    x = gsub("[[:space:]]*$","",x) ## Remove trailing whitespaces
    x = gsub(' +',' ',x) ## Remove extra whitespaces
  })

class(TextPreprocessing)
head(TextPreprocessing)
TextPreprocessing <- paste(unlist(TextPreprocessing), collapse = '')
########## CLEANING THE DATA ############

# let us now lower case this data
clean_data1<- tolower(unlist(TextPreprocessing))
head(clean_data1)

# Cleaning the punctuations, pattern = "\\W"
# We are going to replace the punctuations by space, 
# if we don't do so then we may make new words

clean_data2 <- gsub(pattern = "\\W", replace = " ", clean_data1)
head(clean_data2)

# Cleaning the digits, pattern = "\\d"
# PROBABLY NOT REQUIRED!!

clean_data3 <- gsub(pattern = "\\d", replace = " ", clean_data2)
head(clean_data3)

# Cleaning the stopwords
# install.packages('tm')
library('tm')

#let see a preview of stopwords
stopwords()[1:10]

#let us remove them using function removeWords()
clean_data4 <- removeWords(clean_data3, stopwords())
head(clean_data4)


head(clean_data4)

# let us remove single letters, here \\b[A-z] represents that string starts 
# with any letter between a-z and string can take uppercase letters as well 
# as lower case letters and the subsequent \\b{1} says that the 
# string ends with length one

clean_data5 <- gsub(pattern = "\\b[A-z]\\b{1}", replace = " ", clean_data4)
clean_data5


# We can finally remove whitespaces using 
# stripWhitespace() function, which is part of tm library

clean_data6 <- stripWhitespace(clean_data5)

head(clean_data6)

# install.packages("SnowballC")
# steming

clean_data6 <- stemDocument(clean_data6, language = "english")

head(clean_data6)

###### FREQUENCY OF THE WORDS  ############
# We now have a chunk of lines, and we are looking for counting the words
# If you remember we had joined variouslines and made a chunk
# So we split individual words and add a space between them as splitter

clean_data7<- strsplit(clean_data6," ")

word_freq1<-table(clean_data7)
head(word_freq1)


word_freq2<- cbind(names(word_freq1), as.integer(word_freq1))
head(word_freq2)

write.csv(word_freq2, "Word Frequency5.csv")

###### Word Cloud ########
# we will use word cloud library to come up with word cloud
# install.packages("RColorBrewer")
# install.packages("wordcloud")

library(RColorBrewer)
library(wordcloud)

# After installing packages, 
# we want to organize our words as per wordcloud()

class(clean_data7)

# Class of data stored as words
# We want the class to be as characters
# One way to do is to unlist the list

word_cloud1<-unlist(clean_data7)

class(word_cloud1)


# We want to add colors to words, 
# we chose rainbow function to add multiple colors, 
# number of clors in parentheses
wordcloud(word_cloud1, min.freq = 2, 
          random.order = FALSE, scale = c(5,1), 
          colors = rainbow(7))


##### Sentiment Analysis  ######
# Getting the bags of positive and negative words
# Import positive and negative words bag
positive<- scan("positive.txt", 
                what= "character", 
                comment.char = ";")

negative<- scan("negative.txt", 
                what= "character", 
                comment.char = ";")

# Now we have our positive and negative words
# We will use them to match words in our text, 
# using match()

senti_analysis<- unlist(clean_data7)

match(senti_analysis, positive)[1:40]
match(senti_analysis, negative)[1:40]

# We can see the output, place where it is matching,  we have a number
# This number is representing the position of the word in the list which matches


# Now we shall count the positive and negative words Where so ever NA values are 
# not there, there is a word to be counted Counting and summing them will give total 
# number of positive score Final Sentiment score will be positive-negative score

p_score <- sum(!is.na(match(senti_analysis, positive)))
p_score
n_score <- sum(!is.na(match(senti_analysis, negative)))
n_score

Sentiment_score = p_score - n_score
Sentiment_score

# =================================================================================

# Tweets containing Deepika Padukone-------------
tweetson <- searchTwitter("deepikapadukone", n=1000)
tweetson

tweetson <- twListToDF(tweetson)

# Lets further clean up these tweets. 
# The below code will process your tweets for punctuations, URLs, hashtags, mentions etc. 

TextPreprocessing <- lapply(unlist(tweetson$text), function(x) {
  
  x = gsub('http\\S+\\s*', '', x) ## Remove URLs
  x = gsub('\\b+RT', '', x) ## Remove RT
  x = gsub('#\\S+', '', x) ## Remove Hashtags
  x = gsub('@\\S+', '', x) ## Remove Mentions
  x = gsub('[[:cntrl:]]', '', x) ## Remove Controls and special characters
  x = gsub("\\d", '', x) ## Remove Controls and special characters
  x = gsub('[[:punct:]]', '', x) ## Remove Punctuations
  x = gsub("^[[:space:]]*","",x) ## Remove leading whitespaces
  x = gsub("[[:space:]]*$","",x) ## Remove trailing whitespaces
  x = gsub(' +',' ',x) ## Remove extra whitespaces
})

class(TextPreprocessing)
head(TextPreprocessing)
TextPreprocessing <- paste(unlist(TextPreprocessing), collapse = '')
########## CLEANING THE DATA ############

# let us now lower case this data
clean_data1<- tolower(unlist(TextPreprocessing))
head(clean_data1)

# Cleaning the punctuations, pattern = "\\W"
# We are going to replace the punctuations by space, 
# if we don't do so then we may make new words

clean_data2 <- gsub(pattern = "\\W", replace = " ", clean_data1)
head(clean_data2)

# Cleaning the digits, pattern = "\\d"
# PROBABLY NOT REQUIRED!!

clean_data3 <- gsub(pattern = "\\d", replace = " ", clean_data2)
head(clean_data3)

# Cleaning the stopwords
# install.packages('tm')
library('tm')

#let see a preview of stopwords
stopwords()[1:10]

#let us remove them using function removeWords()
clean_data4 <- removeWords(clean_data3, stopwords())
head(clean_data4)


head(clean_data4)

# let us remove single letters, here \\b[A-z] represents that string starts 
# with any letter between a-z and string can take uppercase letters as well 
# as lower case letters and the subsequent \\b{1} says that the 
# string ends with length one

clean_data5 <- gsub(pattern = "\\b[A-z]\\b{1}", replace = " ", clean_data4)
clean_data5


# We can finally remove whitespaces using 
# stripWhitespace() function, which is part of tm library

clean_data6 <- stripWhitespace(clean_data5)

head(clean_data6)

# install.packages("SnowballC")
# steming

clean_data6 <- stemDocument(clean_data6, language = "english")

head(clean_data6)

###### FREQUENCY OF THE WORDS  ############
# We now have a chunk of lines, and we are looking for counting the words
# If you remember we had joined variouslines and made a chunk
# So we split individual words and add a space between them as splitter

clean_data7<- strsplit(clean_data6," ")

word_freq1<-table(clean_data7)
head(word_freq1)


word_freq2<- cbind(names(word_freq1), as.integer(word_freq1))
head(word_freq2)

write.csv(word_freq2, "Word Frequency5.csv")

###### Word Cloud ########
# we will use word cloud library to come up with word cloud
# install.packages("RColorBrewer")
# install.packages("wordcloud")

library(RColorBrewer)
library(wordcloud)

# After installing packages, 
# we want to organize our words as per wordcloud()

class(clean_data7)

# Class of data stored as words
# We want the class to be as characters
# One way to do is to unlist the list

word_cloud1<-unlist(clean_data7)

class(word_cloud1)


# We want to add colors to words, 
# we chose rainbow function to add multiple colors, 
# number of clors in parentheses
wordcloud(word_cloud1, min.freq = 2, 
          random.order = FALSE, scale = c(5,1), 
          colors = rainbow(7))


##### Sentiment Analysis  ######
# Getting the bags of positive and negative words
# Import positive and negative words bag
positive<- scan("positive.txt", 
                what= "character", 
                comment.char = ";")

negative<- scan("negative.txt", 
                what= "character", 
                comment.char = ";")

# Now we have our positive and negative words
# We will use them to match words in our text, 
# using match()

senti_analysis<- unlist(clean_data7)

match(senti_analysis, positive)[1:40]
match(senti_analysis, negative)[1:40]

# We can see the output, place where it is matching,  we have a number
# This number is representing the position of the word in the list which matches


# Now we shall count the positive and negative words Where so ever NA values are 
# not there, there is a word to be counted Counting and summing them will give total 
# number of positive score Final Sentiment score will be positive-negative score

p_score <- sum(!is.na(match(senti_analysis, positive)))
p_score
n_score <- sum(!is.na(match(senti_analysis, negative)))
n_score

Sentiment_score = p_score - n_score
Sentiment_score







##############################################################################333333333333333333333
