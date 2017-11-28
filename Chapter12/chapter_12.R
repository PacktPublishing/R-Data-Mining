# text data mining
install.packages("tidytext")
library(tidytext)
library(pdftools)
library(dplyr)
library(janeaustenr)
library(stringr)
library(tidyr)
library(ggplot2)
library(wordcloud)
library(igraph)
library(ggraph)
library(tidygraph)
# import companies credit bureau information, stored in PDF format
file_list <- list.files("data")
grepl(".pdf",file_list)

pdf_list <- file_list[grepl(".pdf",file_list)]

# for loop example

for (i in 1:3){
  print(i)
}

vector <- c(1,2,3)

for (i in 1:3){
  vector[i] <- vector[i]+1
}

vector+1
# dataframe from PDF

corpus_raw <- data.frame("company" = c(),"text" = c())

for (i in 1:length(pdf_list)){
print(i)
  pdf_text(paste("data/", pdf_list[i],sep = "")) %>% 
    strsplit("\n")-> document_text
data.frame("company" = gsub(x =pdf_list[i],pattern = ".pdf", replacement = ""), 
           "text" = document_text, stringsAsFactors = FALSE) -> document
print(colnames(document))
colnames(document) <- c("company", "text")
corpus_raw <- rbind(corpus_raw,document)  
}

corpus_raw %>% 
  filter(!grepl("12.05.2017",text)) %>% 
  filter(!grepl("business profile",text)) %>% 
  filter(!grepl("comments",text)) %>%
  filter(!grepl("1",text)) -> corpus

corpus %>% 
  filter(!grepl(c("date of foundation"),text)) %>% 
  filter(!grepl(c( "industry"),text)) %>% 
  filter(!grepl(c( "share holders"),text)) -> comments

corpus %>% 
  filter(grepl(("date of foundation+"),
               text)|grepl(( "industry+"),
                           text)|grepl(( "share holders+"),
                                       text)) -> information
information %>% head()

comments %>% head()

# ON COMMENTS  
comments %>% 
  unnest_tokens(word,text)-> comments_tidy

  
#sentiment analysis  
  
lexicon <- get_sentiments("bing")

comments_tidy %>% 
  inner_join(lexicon) %>% 
  count(company,sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

comments_tidy %>% 
  inner_join(lexicon) %>% 
  count(company,  sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive -negative)-> comments_sentiment

ggplot(comments_sentiment, aes(x = sentiment)) +
  geom_histogram()


# word clouds

comments_tidy %>%
  count(word) %>%
  with(wordcloud(word, n))

comments_tidy %>%
  filter(!word %in% stop_words$word) %>%
  count(word) %>%
  with(wordcloud(word, n))

# ngram analysis 
  comments %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) -> bigram_comments
  
  bigram_comments %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>% 
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>% 
    count(word1, word2, sort = TRUE) 
  
  
  ### trigrams
  
  comments %>% 
    unnest_tokens(trigram, text, token = "ngrams", n = 3) -> trigram_comments
  
  trigram_comments %>%
    separate(trigram, c("word1", "word2","word3"), sep = " ") %>% 
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>% 
    filter(!word3 %in% stop_words$word) %>% 
    count(word1, word2, word3, sort = TRUE) 




## ON INFORMATION

# descriptive: most common industries

information %>% 
  filter(grepl("industry", text)) %>% 
  mutate(industry = gsub("industry: ","",text))-> industries

industries %>% 
  count(industry)  %>% 
  filter(n >1) %>% 
  ggplot(aes(x = industry, y = n)) +
  geom_bar(stat = 'identity')+
  coord_flip()

# network analysis on  shareholders

information %>% 
  filter(grepl("share holders", text)) %>% 
  mutate(shareholders = gsub("share holders: ","",text)) %>% 
  separate(col = shareholders, into = c("first","second","third"),sep = ";") %>% 
  gather(key = "number",value ="shareholder",-company,-text) %>% 
  filter(!is.na(shareholder)) %>% 
  select(company,shareholder)-> shareholders



graph_from_data_frame(shareholders) %>% 
  ggraph() +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, check_overlap = TRUE)+
  theme_graph()

graph_from_data_frame(shareholders)-> graph_object

graph_object %>% 
  ggraph() +
  geom_edge_link(alpha = .2) +
  geom_node_point(alpha =.3) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, check_overlap = TRUE)+
  theme_graph()

#linking the size of a node to its degree
 

 deg <- degree(graph_object, mode="all")
 V(graph_object)$size <- deg*3
 
 set.seed(30)
 graph_object %>% 
   ggraph() +
   geom_edge_link() +
   geom_node_point(aes(size = size),alpha = .3) +
   geom_node_text(aes(label = name,size = size), vjust = 1, hjust = 1, check_overlap = FALSE)+
   theme_graph()
 
 
