install.packages("readxl")
install.packages("wordcloud")
install.packages("SnowballC")
install.packages("RColorBrewer")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("ggplot2")

library("wordcloud")
library("SnowballC")
library("RColorBrewer")
library("ggplot2")
library("dplyr")
library("ggplot2")

library(readxl)
library("tm")

filePath <- "D:/R_Work/Demo_TwitterData.xlsx"
text <- read_excel(filePath)
docs <- Corpus(VectorSource(text))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
dtm <- TermDocumentMatrix(docs)

m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
top_words <- head(d, 10) # get top 10 most occuring words
top_word <- top_words$word
top_freq <- top_words$freq

d %>%
  filter(freq>30)%>%
  mutate(word=reorder(word,freq))%>%
  ggplot(aes(word,freq,fill=word))+
  geom_col()+
  xlab(NULL)+
  coord_flip()