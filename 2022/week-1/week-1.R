#libraries ---------------------------------------------------------------------------------
library(rtweet)
library(dplyr)
library(readtext)
library(tm)

#api variables -----------------------------------------------------------------------------
consumer_key <- "your_consumer_key"
consumer_secret <- "your_consumer_secret_key"
access_token <- "your_access_token"
access_secret <- "your_access_secret_key"

#authorization -----------------------------------------------------------------------------
myAuthorization <- rtweet::create_token(app = "your_project_name",
                                        consumer_key = consumer_key,
                                        consumer_secret = consumer_secret,
                                        access_token = access_token,
                                        access_secret = access_secret)

#scrape tweets -----------------------------------------------------------------------------
myTweets <- rtweet::get_timeline(c("username"), 
                                 n = 100, 
                                 parse = T, 
                                 token = myAuthorization)

#save csv file copy -------------------------------------------------------------------------
rtweet::write_as_csv(myTweets, 
                     "myTweets.csv", 
                     prepend_ids = T, 
                     na = "", 
                     fileEncoding = "UTF-8")

#convert csv to text file -------------------------------------------------------------------
Tweets <- readtext::readtext("myTweets.csv", text_field = "text")

#corpus
tweetCorpus <- Corpus(VectorSource(Tweets$text))

#text cleaning -------------------------------------------------------------------------------
tweetCorpus <- tweetCorpus %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(stripWhitespace) %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removeWords, stopwords("en")) %>% 
  tm_map(removeWords, stopwords("SMART"))

#create tdm ----------------------------------------------------------------------------------
tdm <- TermDocumentMatrix(tweetCorpus) %>% 
  as.matrix()

#get word count
words <- sort(rowSums(tdm), decreasing = T)
worddf <- data.frame(word = names(words), freq = words)

#do more text cleaning to remove unwanted values such as punctuation marks in the data frame

#color palette
twtCol <- c("#14171A", "#F5F8FA", "#E1E8ED", "#FFFF66", "#FF9900")

#plot word cloud -----------------------------------------------------------------------------
wordcloud2::wordcloud2(worddf,
                       color = rep_len(twtCol, nrow(worddf)),
                       backgroundColor = "#1DA1F2",
                       rotateRatio = 0,
                       size = 0.5,
                       minSize = 5)
