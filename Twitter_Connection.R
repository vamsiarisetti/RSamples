# TWITTER APPS URL      :: https://apps.twitter.com/
# Call Back URL         :: http://127.0.0.1:1410
# WEBSITE URL           :: https://twitter.com/taurusvamsi
# TWITTER SETTINGS URL  :: https://twitter.com/settings/your_twitter_data

# install.packages(c("devtools", "rjson", "bit64", "httr"))
# install_github("twitteR", username="geoffjentry")
# install.packages("twitteR")

# require(devtools)
# install_github("lchiffon/wordcloud2")

# install.packages("tm")

# install.packages("stringr")

# install.packages("readxl")
# install.packages("xlsx")

# install.packages("dplyr")
# install.packages("ggplot2")

# install.packages("wordcloud")
# install.packages("SnowballC")
# install.packages("RColorBrewer")

#RESTART R session!

library(devtools)
library(twitteR)

library(wordcloud2)
library(tm)

library(stringr)

library(readxl)
library(xlsx)

library(dplyr)
library(ggplot2)

library(wordcloud)
library(SnowballC)
library(RColorBrewer)

api_key <- "PJuth3uLguBQHiQFUIGb35qCT"
api_secret <- "AsZwwMzhvh4tc8oCyChdAE6xRuaAjuEYzd6hV92xcX8T4gWKL9"
access_token <- "194922040-xAnSdaVU9itcRttFgyJG9oNHkLUmjYjQ98m2aGFF"
access_token_secret <- "LTIXcAQCtdlzzYgGr8TsiBbIopeNOlvrC5cuwBzLOILlP"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
#searchTwitter("iphone")

# tw = twitteR::searchTwitter('#realDonaldTrump + #HillaryClinton', n = 1e4, since = '2016-11-08', retryOnRateLimit = 1e3)
tw = twitteR::searchTwitter('#AgnyaathavasiTeaser', n = 1e4, since = '2016-11-08', retryOnRateLimit = 1e3)
tw_d = twitteR::twListToDF(tw)

# Writing text to xlsx
d_text <- tw_d$text
d_text <- str_replace_all(d_text,"([.\\n<>])","")
data_excel <- write.xlsx(x=d_text,"D:/R_work/Demo.xlsx",sheetName = "Demo",row.names = FALSE)

# Reading text from xlsx
data_text <- read_excel("D:/R_work/Demo.xlsx")

# Cleaning Data
myCorpus <- Corpus(VectorSource(data_text))
inspect(myCorpus)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
myCorpus <- tm_map(myCorpus, toSpace, "/")
myCorpus <- tm_map(myCorpus, toSpace, "@")
myCorpus <- tm_map(myCorpus, toSpace, "\\|")

# Convert the text to lower case
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
myCorpus <- tm_map(myCorpus, removeWords, c(stopwords("english"),"https","tco","sir","ufffd","sufffdufffdufffdufffdu"))
myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, stripWhitespace)
dtm <- TermDocumentMatrix(myCorpus)

m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
top_words <- head(d, 100) # get top 10 most occuring words
top_word <- top_words$word
top_freq <- top_words$freq

d %>%
  filter(freq>1014)%>%
  mutate(word=reorder(word,freq))%>%
  ggplot(aes(word,freq,fill=word))+
  geom_col()+
  xlab(NULL)+
  coord_flip()

# Creating WordCloud2
wordcloud2(data = d)
wordcloud2(d, color = "random-light", backgroundColor = "grey")

figPath = system.file("examples/t.png",package = "wordcloud2")
wordcloud2(d, figPath = figPath, size = 0.75,color = "skyblue")

letterCloud(d, word = "R", size = 2)

letterCloud(d, word = "PK", wordSize = 1)


## Sentiment Analysis

# Libraries for Sentiment Analysis
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
library(plotly)

#data_csv <- write.csv(d_text,file = "D:/R_Work/TwCSV.csv",sep = ",",eol = "\n",col.names =TRUE,row.names = FALSE)

tweet_sent <- iconv(tw_d$text)

senti <- get_nrc_sentiment(tweet_sent)

# get_nrc_sentiment('problem')    # Command to get the sentiment of a word

s_an <- sort(colSums(senti), decreasing = TRUE)
sent_an <- data.frame(sentiment = names(s_an),freq=s_an)

SentByFreq <- sent_an %>% group_by(sentiment, sentiment) %>% summarise(freq)
plot_ly(SentByFreq) %>% add_trace(x=~sentiment, y=~freq,
              type='bar', color=~sentiment) %>% layout(barmode='stack',
              yaxis=list(title='Count'), xaxis=list(title=''),
              title='Sentiment Analysis for Ticket Dump', width=800,
              height=500)