#gives current working directory
getwd()

#you set the directory path where dataset is present
setwd(""~/Downloads")

#install data.table package to access fread function
install.packages("data.table", repos = "https://cran.r-project.org")
library(data.table)

#read the data from psychcentral_data file using fread function
data <- fread("psychcentral_data.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?")) 

#to get number and column Names in data
ncol(data)
colnames(data)

#Number of rows in data
nrow(data)

#Use libraries tidytext and dplyr to tokenize column q_content
install.packages("tidytext", repos = "https://cran.r-project.org")
install.packages("dplyr", repos = "https://cran.r-project.org")
library(tidytext)
library(dplyr)

#add 1 column content_id to data that will have the row number as content id
data$content_id <- seq.int(nrow(data))
head(data, n=5)

#break the text of q_content into tokens for dataset - data
tidy_text<- data %>%
  unnest_tokens(word, q_content) 

#display 1st 20 tokens
tidy_text[1:20]

#removing stop words using anti_join function
data(stop_words)
tidy_text <- tidy_text %>%
  anti_join(stop_words)

#use dplyr's count() to find the most common words 
tidy_text %>%
  count(word, sort = TRUE) 

#library "ggplot2" to create a visualization that shows the frequency of the tokens 
#that appeared for at least 2000 times
library(ggplot2)
tidy_text %>%
  count(word, sort = TRUE) %>%
  filter(n > 2000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

#Snowball package
install.packages("SnowballC", repos = "https://cran.r-project.org")
library(SnowballC)

tidy_text <- data %>%
  unnest_tokens(word, q_content) %>%
  mutate(word = wordStem(word)) 

#removing stop words using anti_join function
data(stop_words)
tidy_text <- tidy_text %>%
  anti_join(stop_words)

#use dplyr's count() to find the most common words 
tidy_text %>%
  count(word, sort = TRUE) 

#library "ggplot2" to create a visualization that shows the frequency of the tokens 
#that appeared for at least 4000 times
library(ggplot2)
tidy_text %>%
  count(word, sort = TRUE) %>%
  filter(n > 2000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

#library "wordcloud" to create a word cloud with the 200 most used tokens
install.packages("wordcloud")
library(wordcloud)

tidy_text %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 200))

#Creating a color-coded word cloud based on sentiment. 
#Use the most frequent 100 tokens for positive and negative words.
install.packages("reshape2", repos = "https://cran.r-project.org")
library(reshape2)

tidy_text %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)
#=======================================================================
#repeat above steps for column "answers"
#=======================================================================
#Use libraries tidytext and dplyr to tokenize column answers
library(tidytext)
library(dplyr)

#add 1 column answers_id to data that will have the row number as answers id
data$answers_id <- seq.int(nrow(data))
head(data, n=5)

#break the text of q_content into tokens for dataset - data
tidy_text1<- data %>%
  unnest_tokens(word, answers) 

#display 1st 20 tokens
tidy_text1[1:20]

#removing stop words using anti_join function
data(stop_words)
tidy_text1 <- tidy_text1 %>%
  anti_join(stop_words)

#use dplyr's count() to find the most common words 
tidy_text1 %>%
  count(word, sort = TRUE) 

#library "ggplot2" to create a visualization that shows the frequency of the tokens 
#that appeared for at least 4000 times
library(ggplot2)
tidy_text1 %>%
  count(word, sort = TRUE) %>%
  filter(n > 4000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

#Snowball package
library(SnowballC)

tidy_text1 <- data %>%
  unnest_tokens(word, answers) %>%
  mutate(word = wordStem(word)) 

#removing stop words using anti_join function
data(stop_words)
tidy_text1 <- tidy_text1 %>%
  anti_join(stop_words)

#use dplyr's count() to find the most common words 
tidy_text1 %>%
  count(word, sort = TRUE) 

#library "ggplot2" to create a visualization that shows the frequency of the tokens 
#that appeared for at least 6000 times
library(ggplot2)
tidy_text1 %>%
  count(word, sort = TRUE) %>%
  filter(n > 2000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

#library "wordcloud" to create a word cloud with the 200 most used tokens
library(wordcloud)

tidy_text1 %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 200))

#Creating a color-coded word cloud based on sentiment. 
#Use the most frequent 100 tokens for positive and negative words.
library(reshape2)

tidy_text1 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)

#============================================================
#Topic-modeling on q_content
#============================================================
install.packages("tm")
install.packages("topicmodels")
install.packages("slam")
install.packages("RTextTools", repos = "https://cran.r-project.org")
library(tm)
library(wordcloud)
library(topicmodels)
library(slam)
library(RTextTools)
data <- data[1:1000,] # We perform LDA on the rows 1 through 1000 in the data.
corpus <- Corpus(VectorSource(data$q_content), readerControl=list(language="en"))
dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minWordLength = 2, removeNumbers = TRUE, removePunctuation = TRUE,  stemDocument = TRUE))
dtm
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ] #remove all docs without words
lda <- LDA(dtm.new, k = 5) # k is the number of topics to be found.

wordstoremove <- c("dont","every","always", "back", "going","things", "will","never","feel","think","like", "just","people","one","can","want","really","now","much","something","still","just","ive","didnt","even","get","may","also")
data_sub <- as.data.frame(sapply(data, function(x) gsub(paste(wordstoremove, collapse = '|'), '', x)))
corpus <- Corpus(VectorSource(data_sub$q_content), readerControl=list(language="en"))
q_content_dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minWordLength = 1, removeNumbers = TRUE, removePunctuation = TRUE))
dim(q_content_dtm)

#lda <- LDA(dtm.new, k = 2) 
#lda <- LDA(dtm.new, k = 3) 
#lda <- LDA(dtm.new, k = 4) 
#lda <- LDA(dtm.new, k = 10) 
lda <- LDA(q_content_dtm, k = 5)
lda <- LDA(q_content_dtm, k = 2) 
lda <- LDA(q_content_dtm, k = 3) 
lda <- LDA(q_content_dtm, k = 4) 
lda <- LDA(q_content_dtm, k = 10) 

library(tidytext)
lda_td <- tidy(lda)
lda_td

library(ggplot2)
library(dplyr)
top_terms <- lda_td %>%
  group_by(topic) %>%
  # top_n to find the 10 terms that are most common within each topic
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
#============================================================
#Topic-modeling on answers
#============================================================
library(tm)
library(wordcloud)
library(topicmodels)
library(slam)
library(RTextTools)
data <- data[1:1000,] # We perform LDA on the rows 1 through 1000 in the data.
corpus <- Corpus(VectorSource(data$answers), readerControl=list(language="en"))
dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minWordLength = 2, removeNumbers = TRUE, removePunctuation = TRUE,  stemDocument = TRUE))
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ] #remove all docs without words
lda <- LDA(dtm.new, k = 5) # k is the number of topics to be found.

wordstoremove <- c("dont","way", "make", "youre","see", "every","always", "back", "going","things", "will","never","feel","think","like", "just","people","one","can","want","really","now","much","something","still","just","ive","didnt","even","get","may","also")
data_sub <- as.data.frame(sapply(data, function(x) gsub(paste(wordstoremove, collapse = '|'), '', x)))
corpus <- Corpus(VectorSource(data_sub$answers), readerControl=list(language="en"))
answers_dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minWordLength = 1, removeNumbers = TRUE, removePunctuation = TRUE))
dim(answers_dtm)
rowTotals <- apply(answers_dtm , 1, sum) #Find the sum of words in each Document
answers_dtm   <- answers_dtm[rowTotals> 0, ] #remove all docs without words

lda <- LDA(answers_dtm, k = 5)
lda <- LDA(answers_dtm, k = 2) 
lda <- LDA(answers_dtm, k = 8) 
lda <- LDA(answers_dtm, k = 11) 
lda <- LDA(answers_dtm, k = 14) 

library(tidytext)
lda_td <- tidy(lda)
lda_td

library(ggplot2)
library(dplyr)
top_terms <- lda_td %>%
  group_by(topic) %>%
  # top_n to find the 10 terms that are most common within each topic
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
