

####### Sentiment analysis of tweets #########

# Work directory
setwd("D:/FCD/Projetos/Sentiment_analysis")

library(twitteR)
library(dplyr)
library(ggplot2)
library(tm)
library(RColorBrewer)
library(wordcloud)

###### Collecting and cleaning tweets #######

## Conecting to twitter

# Authentication keys
ck = 'xxxxxxxxxxxxxxxxxxxx'
cs = 'xxxxxxxxxxxxxxxxxxxx'
at = 'xxxxxxxxxxxxxxxxxxxx'
as = 'xxxxxxxxxxxxxxxxxxxx'

# Connection
setup_twitter_oauth(consumer_key = ck,
                    consumer_secret = cs,
                    access_token = at,
                    access_secret = as)

## Searching for tweets

# keywords and hashtags to search through
terms <- c('Data science','data scientist')

# syntax used in searchTwitter() for multiple search terms.
terms_search <- paste(terms, collapse = ' OR ')

# Search
?searchTwitteR
tweets <- searchTwitter(terms_search, n = 3000, lang = 'en')

## Creating a dataframe with the tweets collected
df <- do.call("rbind", lapply(tweets, as.data.frame))
View(df)
str(df)
dim(df)

## Saving the dataset
write.csv(df, 'tweets.csv', row.names = FALSE)

## Analysing the collected data

# Number of tweets per user
df %>% group_by(screenName) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>%
  head(20) %>%
  ggplot(aes(x = n, y = reorder(screenName, n))) +
  geom_bar(stat = 'identity', color = 'orchid4', fill = 'orchid3') +
  ggtitle('Top 20 users in the dataset') +
  xlab('Number of tweets') +
  ylab('')


## Cleaning the text

# Selecting only the text column
dftweet <- df %>% select(text)
View(dftweet)

# Function to clean the text
clean_tweets <- function(char_vector){
  # Remove http links
  char_vector = gsub('http\\S+', '', char_vector)
  # Remove retweets
  char_vector = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", char_vector)
  # Remove “#Hashtag”
  char_vector = gsub("#\\w+", " ", char_vector)
  # Remove username
  char_vector = gsub('@\\S+', '', char_vector)
  # Remove emoticons
  char_vector = gsub("[^\x01-\x7F]", "", char_vector)
  # Transform non-UTF8 characters
  # char_vector = stringi::stri_trans_general(char_vector, "latin-ascii")
  # Remove punctuation
  char_vector = gsub('[[:punct:]]', ' ', char_vector)
  # Remove extra spaces
  char_vector = gsub("[ \t]{2,}", " ", char_vector)
  char_vector = gsub("^\\s+|\\s+$", "", char_vector)
  # Remove numbers
  char_vector = gsub("[[:digit:]]", "", char_vector)
  
  return(char_vector)
}

dftweet$text <- clean_tweets(dftweet[['text']])

# Looking for NA
sum(is.na(dftweet)) 

## Looking for duplicated tweets

# 1643 tweets were duplicated
length(dftweet[duplicated(dftweet),])

# Removing duplicated tweets
dftweet <- dftweet[!duplicated(dftweet),]

# Salvando os tweets em um arquivo csv
write.csv(dftweet, 'tweets_clean.csv', row.names = FALSE)


########## Text mining in R ########

# Reading the data 
tweets <- read.csv('tweets_clean.csv', stringsAsFactors = FALSE)
View(tweets)

# It is important to point that the cleaning procedure performed previously is not necessary, because
# R has packages for that purpose, like tm and tidytext, which would to the same job for us. However,
# for learning purpose, I believe is very useful for proper comprehension to code the processes that
# are used in text mining, instead of just use the packages (personal opinion).

# In tm package, the main type of object is the corpus object, which organizes text as a collection of
# documents (books, website, email or even a sentences) and can be worked with like a list.

# Tidytext, on the other hand, utilizes a dataframe as its data structure.

# Creating a corpus
corpus <- tm::VCorpus(tm::VectorSource(tweets$x))

# The corpus is a list.
str(corpus[1])
print(corpus[1])

# Inspect corpus returns the number of chars that compose the document
tm::inspect(corpus[1])

# Printing the text
substring(corpus[[1]]$content, 1, 95)
# OR
as.character(corpus[[1]])
# print more than 1 line
text <- lapply(corpus[1:5], as.character)
text

# As can be seen, the punctuation was removed earlier, but there are some desired modifications to make,
# as removing the stopwords, removing some metacharacters like \n, stem words. Let's do that using 
# tm_map()

corpus <- tm_map(corpus, content_transformer(tolower))
lapply(corpus[1:5], as.character)

# Removing stopwords
corpus <- tm_map(corpus, removeWords, stopwords('english'))
lapply(corpus[1:5], as.character)

# Other cleaning procedures
replaceword <- content_transformer(function(x, pattern, replace){
  return(gsub(pattern, replace, x))
})

corpus <- tm_map(corpus, replaceword, '\\s[s]\\s', '')
corpus <- tm_map(corpus, replaceword, '\n+', '')
corpus <- tm_map(corpus, replaceword, 'scientisttoolbox', 'scientist toolbox')
corpus <- tm_map(corpus, stripWhitespace)
lapply(corpus[1:5], as.character)

# Word stemming
corpus_stem <- tm_map(corpus, stemDocument)
lapply(corpus_stem[1:5], as.character)

## Creating a wordcloud viz
wordcloud(corpus, min.freq = 10, colors = brewer.pal(8,"Dark2"), random.order = FALSE)

## Creating a document term matrix
# A document term matrix give us an idea of the counts of words.
sparceMatrix <- DocumentTermMatrix(corpus)
inspect(sparceMatrix[1:10,10:50])
words_freq <- colSums(as.matrix(sparceMatrix))
orden <- order(words_freq, decreasing = TRUE)
head(words_freq[orden],10)

# Sentiment inspect of the tweets collected. For this analysis, I will use the original words, without
# stem.
require(dplyr)

tidytext::get_sentiments('nrc')
tidytext::get_sentiments('bing')
get_sentiments('afinn')
# Transform the vector of words from words_freq in a tibble.
tidy_text <- as_tibble(names(words_freq[orden])) %>% rename(word = value)

tidy_text %>% inner_join(tidytext::get_sentiments('nrc')) %>%
  count(sentiment, sort = TRUE) %>%
  ggplot(aes(x = reorder(sentiment, -n), y = n, fill = sentiment)) +
  geom_bar(stat = 'identity') +
  xlab('') +
  ylab('') +
  ggtitle('Sentiment analysis with NRC')

tidy_text %>% inner_join(tidytext::get_sentiments('bing')) %>%
  count(sentiment, sort = TRUE) %>%
  ggplot(aes(x = reorder(sentiment, -n), y = n, fill = sentiment)) +
  geom_bar(stat = 'identity', width = 0.3) +
  xlab('') +
  ylab('') +
  ggtitle('Sentiment analysis with Bing')


  
