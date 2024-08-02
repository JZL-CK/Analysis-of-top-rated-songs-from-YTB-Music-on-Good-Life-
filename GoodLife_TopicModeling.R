
#Set directory 

setwd("E:/Research work/GoodLife-MusicAnalysis")


#Libraries

install.packages("topicmodels")
install.packages("pdftools")
install.packages("tm")
install.packages("tidytext")
install.packages("ggplot2")
install.packages("dplyr")

library(topicmodels)
library(pdftools)
library(tm)
library(tidytext)
library(ggplot2)
library(dplyr)


#Load pdf files

Files <- list.files(pattern = "pdf$")
lyrics <- lapply(Files, pdf_text)

lyrics[1]
lyrics[2]
lyrics[3]
lyrics[4]

length(lyrics)
lapply(lyrics, length)


#Creating corpus

document <- Corpus(VectorSource(lyrics))

document <- tm_map(document, content_transformer(tolower)) #convert to lower case
document <- tm_map(document, removeNumbers) #remove numbers
document <- tm_map(document, removeWords, stopwords("english")) #remove stopwords
document <- tm_map(document, removePunctuation, preserve_intra_word_dashes = TRUE) #remove punctuations
document <- tm_map(document, stripWhitespace) #remove whitespace


#Creating Document Term Matrix

DTM <- DocumentTermMatrix(document)


#Creating LDA Model with 4 Topics

Model_lda <- LDA(DTM, k = 4, control = list(seed = 1234))
Model_lda


#Beta values (Probability of a word being associated with a topic)

beta_topics <- tidy(Model_lda, matrix = 'beta') #creating the beta model 
beta_topics


#Grouping Terms by Topic

beta_top_terms <- beta_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)

#Visualizing the Grouped Terms

beta_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = 'free') +
  scale_y_reordered()

#Filtering terms by topics

tidy(DTM) %>%
  filter(document ==3) %>%
  arrange(desc(count))

#Examining per document per topic probability 

gamma_documents <- tidy(Model_lda, matrix = "gamma")
gamma_documents


#Creating Dataframe with gamma results

doc_gamma.df <- data.frame(gamma_documents)
doc_gamma.df$chapter <- rep(1:dim(DTM)[1],2)

#Plotting gamma results 

ggplot(data = doc_gamma.df, aes(x = chapter, y = gamma,
                                group = factor(topic), color = factor(topic)))+
  geom_line() + facet_wrap(~factor(topic), ncol = 1)
