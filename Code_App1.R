#Data cleaning
library(lubridate)
library(tidyverse)
library(tidytext)
library(stringr)
library(tidytext)
library(tidyr)
library(stopwords)
#Stemming
library(sentimentr)
library(SnowballC)
#Emotions
library(syuzhet)
#POS
library(udpipe)
#Visualization
library(ggplot2)
library(gridExtra)
library(extrafont)
library(RColorBrewer)
library(pdp)
#LDA
library(LDAvis)
library(tidytext)
library(topicmodels)
library(textmineR)
library(slam)

#Models
library(glmnet)
library(randomForest)
library(caret)


Application <- read.csv("C:/Users/gergo/Desktop/Github/MasterThesisFinal/Application1.csv", sep=";")


Application<- Application[,c(9,10,12)]


Application$Time<- ymd(Application$Time)
Application$Time<- year(Application$Time)

Application<- Application[Application$Time>2017,]
Application<- Application[!duplicated(Application$Comments),]


Num_of_Reviews<-Application%>%
  group_by(Time)%>%
  summarise(total_count=n())
Num_of_Reviews<- na.omit(Num_of_Reviews)

Application$Comments <- gsub("[^a-zA-Z0-9.,!?;:]", " ", Application$Comments)
Application$Comments <- gsub("\\s+", " ", Application$Comments)


Application <- Application %>%
  mutate(Comments = str_replace_all(Comments, " t ", " not "),
         Comments = str_replace_all(Comments, " s ", " is "),
         Comments = str_replace_all(Comments, " ve ", " have "),
         Comments = str_replace_all(Comments, " won ", " will "),
         Comments = str_replace_all(Comments, " don ", " do "),
         Comments = str_replace_all(Comments, " m ", " am "),
         Comments = str_replace_all(Comments, "Don ", "Do "),
         Comments = str_replace_all(Comments, "Doesn ", "Does "),
         Comments = str_replace_all(Comments, "doesn ", "Does "),
         Comments = str_replace_all(Comments, " doesnt ", " does not "),
         Comments = str_replace_all(Comments, "Doesnt ", "Does not "),
         Comments = str_replace_all(Comments, "Doesn ", "Does "),
         Comments = str_replace_all(Comments, "Dont ", "Do not "),
         Comments = str_replace_all(Comments, " Dont ", " Do not "),
         Comments = str_replace_all(Comments, " dont ", " do not "),
         Comments = str_replace_all(Comments, "don ", "Do not "),
         Comments = str_replace_all(Comments, " wont ", " will not "),
         Comments = str_replace_all(Comments, " havent ", " have not "),
         Comments = str_replace_all(Comments, " haven ", " have "),
         Comments = str_replace_all(Comments, " Haven ", " have "),
         Comments = str_replace_all(Comments, "Haven ", " have "),
         Comments = str_replace_all(Comments, " Havent ", " Have not "),
         Comments = str_replace_all(Comments, " cant ", " can not "),
         Comments = str_replace_all(Comments, " Cant ", " Can not "),
         Comments = str_replace_all(Comments, " couldn ", " could not "),
         Comments = str_replace_all(Comments, " Couldn ", " could not "),
         Comments = str_replace_all(Comments, " re ", " are "),
         Comments = str_replace_all(Comments, " aren ", " are "),
         Comments = str_replace_all(Comments, " isn ", " is "),
         Comments = str_replace_all(Comments, " n ", " and "),
         Comments = str_replace_all(Comments, " d ", " would "),
         Comments = str_replace_all(Comments, " wouldn ", " would "),
         Comments = str_replace_all(Comments, " Wouldn ", " would "),
         Comments = str_replace_all(Comments, " wouldnt ", " would not "),
         Comments = str_replace_all(Comments, " couldnt ", " could not "),
         Comments = str_replace_all(Comments, " didn ", " did not "),
         Comments = str_replace_all(Comments, " Didn ", " did not "),
         Comments = str_replace_all(Comments, "Didn ", "Did not "),
         Comments = str_replace_all(Comments, "didn ", "did not "),
         Comments = str_replace_all(Comments, "didnt ", "did not "),
         Comments = str_replace_all(Comments, "Didnt ", "Did not "),
         Comments = str_replace_all(Comments, " didnt ", "did not "),
         Comments = str_replace_all(Comments, " Didnt ", " did not "),
         Comments = str_replace_all(Comments, " wasn ", " was not "),
         Comments = str_replace_all(Comments, " wasnt ", " was not "),
         Comments = str_replace_all(Comments, " Wasn ", " was not "),
         Comments = str_replace_all(Comments, " Wasnt ", " was not "),
         Comments = str_replace_all(Comments, "wasn ", "Was not "),
         Comments = str_replace_all(Comments, "Wasn ", "Was not "),
         Comments = str_replace_all(Comments, "wasnt ", "Was not "),
         Comments = str_replace_all(Comments, "Wasnt ", "Was not "),
         Comments = str_replace_all(Comments, " cannot ", " can not "),
         Comments = str_replace_all(Comments, "Cannot ", "Can not "),
         Comments = str_replace_all(Comments, " Cannot ", " Can not "),
         Comments = str_replace_all(Comments, " ll ", " will "),
         Comments = str_replace_all(Comments, " hasn ", " has not "),
         Comments = str_replace_all(Comments, " hasnt ", " has not "),
         Comments = str_replace_all(Comments, " Hasn ", " has not "),
         Comments = str_replace_all(Comments, "Hasn ", "Has not "),
         Comments = str_replace_all(Comments, "Hadn ", "Had not "),
         Comments = str_replace_all(Comments, " hadn ", " had not "),
         Comments = str_replace_all(Comments, " Hadn ", " Had not "),
         Comments = str_replace_all(Comments, " doesn ", " does "))

Application$Comments <- as.character(Application$Comments)  %>%
  {gsub("(\"| |\\$)-+\\.-+"," NUMBER", .)} %>%      
  {gsub("([0-9]+:)*[0-9]+ *am"," TIME_AM", .)} %>%  
  {gsub("([0-9]+:)*[0-9]+ *pm"," TIME_PM", .)} %>%  
  {gsub("-+:-+","TIME", .)} %>%                     
  {gsub("\\$ ?[0-9]*[\\.,]*[0-9]+"," DOLLARVALUE ", .)} %>%   
  {gsub("[0-9]*[\\.,]*[0-9]+"," NUMBER ", .)} %>%  
  {gsub("-"," ", .)} %>%                          
  {gsub("&"," and ", .)} %>%                       
  {gsub("\"+"," ", .)} %>%                         
  {gsub("\\|+"," ", .)} %>%                        
  {gsub("_+"," ", .)} %>%                          
  {gsub(";+"," ", .)} %>%                          
  {gsub(" +"," ", .)} %>%
  {gsub("'+"," ", .)} %>%
  {gsub("\\.+","\\.", .)}

Application<- Application[!Application$Comments==" ",]
Application <- Application %>%
  mutate(Comments = str_replace_all(Comments, " not not ", " not "),
         Comments = str_replace_all(Comments, " NUMBER NUMBER ", " NUMBER "),
         Comments = str_replace_all(Comments, " NUMBER NUMBER NUMBER ", " NUMBER "))


stop_words <- stopwords("en")
stop_words <- stop_words[!(stop_words %in% c("no", "not", "nor"))]
additional_stop_words <- c("app", "apps", "App", "Apps", "mileage" ,"Mileage")
stop_words <- unique(c(stop_words, additional_stop_words))

remove_stopwords <- function(text) {
  words <- unlist(strsplit(text, "\\s+"))
  filtered_words <- words[!tolower(words) %in% stop_words]
  return(paste(filtered_words, collapse = " "))
}

Application<- cbind(ID=1:nrow(Application),Application)
Application$Comments_NOSTOP <- sapply(Application$Comments, remove_stopwords)
Application <- Application %>%
  mutate(Comments_NOSTOP = tolower(Comments_NOSTOP), 
         Comments_NOSTOP = str_replace_all(Comments_NOSTOP, "[,!.?;:]", ""))
Application<- Application[!Application$Comments_NOSTOP==" ",]


Application$Sentiment <-sentiment(Application$Comments_NOSTOP, lexicon::hash_sentiment_huliu)$sentiment



stemmed_comments <- c()
for (j in 1:nrow(Application)) {
  tokenized_comments <- unnest_tokens(Application[j,], word, Comments_NOSTOP, drop = FALSE, to_lower = TRUE)
  stemmed_words <- wordStem(tokenized_comments[["word"]], language = "en")
  stemmed_comment <- paste(stemmed_words, collapse = " ")
  stemmed_comments <- c(stemmed_comments, stemmed_comment)
}
Application$Comments_NOSTOP_STEMMED <- stemmed_comments
Application <- Application %>%
  mutate(Comments_NOSTOP_STEMMED = str_replace_all(Comments_NOSTOP_STEMMED, " number number ", " number "))

##############################################################  BIGRAMS

bigrams <- Application[,c(1,2,3,6,7)] %>%
  unnest_tokens(bigram, Comments_NOSTOP_STEMMED, token = "ngrams", n = 2)

bigrams<- na.omit(bigrams)

frequent_bigrams<- bigrams %>%
  count(bigram, sort = T)

top_freq_bigram<-frequent_bigrams[frequent_bigrams$n>19,]#####################CHANGE 1% of Reviews

##############################################################  TRIGRAMS
trigrams <- Application[,c(1,2,3,6,7)] %>%
  unnest_tokens(trigram, Comments_NOSTOP_STEMMED, token = "ngrams", n = 3)

trigrams<- na.omit(trigrams)

frequent_trigrams<- trigrams %>%
  count(trigram, sort = T)

top_freq_trigram<-frequent_trigrams[frequent_trigrams$n>19,]#####################CHANGE 1% of Reviews


colnames<- (c(top_freq_bigram$bigram,top_freq_trigram$trigram))
colnames_checker <- data.frame(matrix(NA, nrow = nrow(Application), ncol = length(colnames)))
colnames(colnames_checker)<-colnames

Application<- cbind(Application,colnames_checker)

for (i in 8:ncol(Application)) {
  Application[,i]<-  grepl(colnames(Application)[i], Application$Comments_NOSTOP_STEMMED, ignore.case = TRUE)
}


############################################################### EMOTIONS

nrc_sentiment_key <- syuzhet:::nrc %>%
  dplyr::filter(
    sentiment %in% c('positive', 'negative'),
    lang == 'english'
  ) %>% 
  dplyr::select(-lang) %>%
  mutate(value = ifelse(sentiment == 'negative', value * -1, value)) %>% 
  dplyr::group_by(word) %>%
  dplyr::summarize(y = mean(value)) %>% 
  sentimentr::as_key()

nrc_anger_key <- syuzhet:::nrc %>%
  dplyr::filter(
    sentiment %in% c('anger'),
    lang == 'english'
  ) %>% 
  dplyr::select(-lang) %>% 
  dplyr::group_by(word) %>%
  dplyr::summarize(y = mean(value)) %>% 
  sentimentr::as_key()

nrc_joy_key <- syuzhet:::nrc %>%
  dplyr::filter(
    sentiment %in% c('joy'),
    lang == 'english'
  ) %>% 
  dplyr::select(-lang) %>% 
  dplyr::group_by(word) %>%
  dplyr::summarize(y = mean(value)) %>%
  sentimentr::as_key()

reviews_classified <- get_nrc_sentiment(Application[,"Comments_NOSTOP"])
reviews_classified <- cbind(Application[,c(1,2,3,4,6)],reviews_classified)


Application<- cbind(Application, reviews_classified[,c(6:13)])

######################################################################################## POS

ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
comments_udpipe <- udpipe_annotate(ud_model, x = Application$Comments_NOSTOP, doc_id = Application$ID)
pos_tags <- as.data.frame(comments_udpipe) 
pos_tags <- pos_tags[,c(1,7,8)]
colnames(pos_tags)[1]<-"ID"
POS_words<- merge(reviews_classified,pos_tags, by="ID")
POS_words$negSUM<- POS_words$anger+POS_words$disgust+POS_words$fear+POS_words$sadness
POS_words$posSUM<- POS_words$anticipation+POS_words$joy+POS_words$trust




POS_words<- POS_words[!POS_words$lemma=="app",]
POS_words$Sumsumsum<- POS_words$negSUM+POS_words$posSUM
mean(POS_words$Sumsumsum)
POS_positive_nouns<- POS_words[(POS_words$negSUM<2)&(POS_words$posSUM>1) & (POS_words$upos=="NOUN"),c(1,2,3,5,16,17)]
POS_negative_nouns<- POS_words[(POS_words$negSUM>1)&(POS_words$posSUM<2) & (POS_words$upos=="NOUN"),c(1,2,3,5,16,17)]


POS_positive_verbs<- POS_words[(POS_words$negSUM<2)&(POS_words$posSUM>1) & (POS_words$upos=="VERB"),c(1,2,3,5,16,17)]
POS_negative_verbs<- POS_words[(POS_words$negSUM>1)&(POS_words$posSUM<2) & (POS_words$upos=="VERB"),c(1,2,3,5,16,17)]

#######################################################################################################################################2019
all_nouns19<-full_join(POS_positive_nouns %>% filter(Time==2019)%>%count(lemma, sort=TRUE) %>% filter(n>10),
                       POS_negative_nouns %>%filter(Time==2019) %>% count(lemma, sort=TRUE) %>% filter(n>10),
                       by="lemma")

all_verbs19<-full_join(POS_positive_verbs %>%filter(Time==2019)%>% count(lemma, sort=TRUE) %>% filter(n>10),
                       POS_negative_verbs %>%filter(Time==2019)%>% count(lemma, sort=TRUE) %>% filter(n>10),
                       by="lemma")


all_nouns19 = rename(all_nouns19, "positive_count" = "n.x", "negative_count" = "n.y")
all_nouns19[is.na(all_nouns19$positive_count), "positive_count"]<- 0 
all_nouns19[is.na(all_nouns19$negative_count), "negative_count"]<- 0 
all_nouns19$positive_count  <- all_nouns19$positive_count/sum(all_nouns19$positive_count)
all_nouns19$negative_count  <- all_nouns19$negative_count/sum(all_nouns19$negative_count)
all_nouns19$diff <- all_nouns19$positive_count-all_nouns19$negative_count

n19 <- all_nouns19 %>%
  mutate(word = reorder(lemma, diff)) %>%
  top_n(30, diff) %>%
  ggplot(aes(word, diff, fill = diff)) +  
  geom_hline(yintercept = 0, color = "lightgray", linetype = "solid") +
  geom_col(width = 0.25) +
  labs(x = NULL, y = NULL, fill = NULL) +
  coord_flip() +
  scale_fill_gradient2(low = "red", mid = "orange", high = "darkgreen",limits = c(-1, 1), midpoint = 0) +
  ggtitle("Specific Negative/Positive Nouns in 2019")+
  theme_classic(base_family = "Century")+
  scale_y_continuous(limits = c(-1, 1))+
  theme(legend.position = "none") 

##########################################################VERBS

all_verbs19 = rename(all_verbs19, "positive_count" = "n.x", "negative_count" = "n.y")
all_verbs19[is.na(all_verbs19$positive_count), "positive_count"]<- 0 
all_verbs19[is.na(all_verbs19$negative_count), "negative_count"]<- 0 
all_verbs19$positive_count  <- all_verbs19$positive_count/sum(all_verbs19$positive_count)
all_verbs19$negative_count  <- all_verbs19$negative_count/sum(all_verbs19$negative_count)
all_verbs19$diff <- all_verbs19$positive_count-all_verbs19$negative_count
v19 <- all_verbs19 %>%
  mutate(word = reorder(lemma, positive_count)) %>%
  top_n(30, positive_count) %>%
  ggplot(aes(word, positive_count, fill = positive_count)) +
  geom_hline(yintercept = 0, color = "lightgray", linetype = "solid") +
  geom_col(width = 0.25) +
  labs(x = NULL, y = NULL, fill = NULL) +
  coord_flip() +
  theme(text = element_text(size = 17),) +
  scale_fill_gradient2(low = "red", mid = "orange", high = "darkgreen",limits = c(-1, 1), midpoint = 0) +
  ggtitle("Specific Negative/Positive Verbs in 2019")+
  theme_classic(base_family = "Century")+
  scale_y_continuous(limits = c(-1, 1))


grid.arrange(n19, v19, nrow = 1)

#######################################################################################################################################2020
all_nouns20<-full_join(POS_positive_nouns %>% filter(Time==2020)%>%count(lemma, sort=TRUE) %>% filter(n>10),
                       POS_negative_nouns %>%filter(Time==2020) %>% count(lemma, sort=TRUE) %>% filter(n>10),
                       by="lemma")

all_verbs20<-full_join(POS_positive_verbs %>%filter(Time==2020)%>% count(lemma, sort=TRUE) %>% filter(n>10),
                       POS_negative_verbs %>%filter(Time==2020)%>% count(lemma, sort=TRUE) %>% filter(n>10),
                       by="lemma")


all_nouns20 = rename(all_nouns20, "positive_count" = "n.x", "negative_count" = "n.y")
all_nouns20[is.na(all_nouns20$positive_count), "positive_count"]<- 0 
all_nouns20[is.na(all_nouns20$negative_count), "negative_count"]<- 0 
all_nouns20$positive_count  <- all_nouns20$positive_count/sum(all_nouns20$positive_count)
all_nouns20$negative_count  <- all_nouns20$negative_count/sum(all_nouns20$negative_count)
all_nouns20$diff <- all_nouns20$positive_count-all_nouns20$negative_count

n20 <- all_nouns20 %>%
  mutate(word = reorder(lemma, positive_count)) %>%
  top_n(30, positive_count) %>%
  ggplot(aes(word, positive_count, fill = positive_count)) +
  geom_hline(yintercept = 0, color = "lightgray", linetype = "solid") +
  geom_col(width = 0.5) +
  labs(x = NULL, y = NULL, fill = NULL) +
  coord_flip() +
  scale_fill_gradient2(low = "red", mid = "orange", high = "darkgreen",limits = c(-1, 1), midpoint = 0) +
  ggtitle("Specific Negative/Positive Nouns in 2020")+
  theme_classic(base_family = "Century")+
  scale_y_continuous(limits = c(-1, 1))+
  theme(legend.position = "none") 

##########################################################VERBS

all_verbs20 = rename(all_verbs20, "positive_count" = "n.x", "negative_count" = "n.y")
all_verbs20[is.na(all_verbs20$positive_count), "positive_count"]<- 0 
all_verbs20[is.na(all_verbs20$negative_count), "negative_count"]<- 0 
all_verbs20$positive_count  <- all_verbs20$positive_count/sum(all_verbs20$positive_count)
all_verbs20$negative_count  <- all_verbs20$negative_count/sum(all_verbs20$negative_count)
all_verbs20$diff <- all_verbs20$positive_count-all_verbs20$negative_count
v20 <- all_verbs20 %>%
  mutate(word = reorder(lemma, positive_count)) %>%
  top_n(30, positive_count) %>%
  ggplot(aes(word, positive_count, fill = positive_count)) +
  geom_hline(yintercept = 0, color = "lightgray", linetype = "solid") +
  geom_col(width = 0.3) +
  labs(x = NULL, y = NULL, fill = NULL) +
  coord_flip() +
  theme(text = element_text(size = 17),) +
  scale_fill_gradient2(low = "red", mid = "orange", high = "darkgreen",limits = c(-1, 1), midpoint = 0) +
  ggtitle("Specific Negative/Positive Verbs in 2020")+
  theme_classic(base_family = "Century")+
  scale_y_continuous(limits = c(-1, 1))


grid.arrange(n20, v20, nrow = 1)
#######################################################################################################################################2021
all_nouns21<-full_join(POS_positive_nouns %>% filter(Time==2021)%>%count(lemma, sort=TRUE) %>% filter(n>10),
                       POS_negative_nouns %>%filter(Time==2021) %>% count(lemma, sort=TRUE) %>% filter(n>10),
                       by="lemma")

all_verbs21<-full_join(POS_positive_verbs %>%filter(Time==2021)%>% count(lemma, sort=TRUE) %>% filter(n>10),
                       POS_negative_verbs %>%filter(Time==2021)%>% count(lemma, sort=TRUE) %>% filter(n>10),
                       by="lemma")


all_nouns21 = rename(all_nouns21, "positive_count" = "n.x", "negative_count" = "n.y")
all_nouns21[is.na(all_nouns21$positive_count), "positive_count"]<- 0 
all_nouns21[is.na(all_nouns21$negative_count), "negative_count"]<- 0 
all_nouns21$positive_count  <- all_nouns21$positive_count/sum(all_nouns21$positive_count)
all_nouns21$negative_count  <- all_nouns21$negative_count/sum(all_nouns21$negative_count)
all_nouns21$diff <- all_nouns21$positive_count-all_nouns21$negative_count

n21 <- all_nouns21 %>%
  mutate(word = reorder(lemma, positive_count)) %>%
  top_n(30, positive_count) %>%
  ggplot(aes(word, positive_count, fill = positive_count)) +
  geom_hline(yintercept = 0, color = "lightgray", linetype = "solid") +
  geom_col(width = 0.45) +
  labs(x = NULL, y = NULL, fill = NULL) +
  coord_flip() +
  scale_fill_gradient2(low = "red", mid = "orange", high = "darkgreen",limits = c(-1, 1), midpoint = 0) +
  ggtitle("Specific Negative/Positive Nouns in 2021")+
  theme_classic(base_family = "Century")+
  scale_y_continuous(limits = c(-1, 1))+
  theme(legend.position = "none") 

##########################################################VERBS

all_verbs21 = rename(all_verbs21, "positive_count" = "n.x", "negative_count" = "n.y")
all_verbs21[is.na(all_verbs21$positive_count), "positive_count"]<- 0 
all_verbs21[is.na(all_verbs21$negative_count), "negative_count"]<- 0 
all_verbs21$positive_count  <- all_verbs21$positive_count/sum(all_verbs21$positive_count)
all_verbs21$negative_count  <- all_verbs21$negative_count/sum(all_verbs21$negative_count)
all_verbs21$diff <- all_verbs21$positive_count-all_verbs21$negative_count
v21 <- all_verbs21 %>%
  mutate(word = reorder(lemma, positive_count)) %>%
  top_n(30, positive_count) %>%
  ggplot(aes(word, positive_count, fill = positive_count)) +
  geom_hline(yintercept = 0, color = "lightgray", linetype = "solid") +
  geom_col(width = 0.43) +
  labs(x = NULL, y = NULL, fill = NULL) +
  coord_flip() +
  theme(text = element_text(size = 17),) +
  scale_fill_gradient2(low = "red", mid = "orange", high = "darkgreen",limits = c(-1, 1), midpoint = 0) +
  ggtitle("Specific Negative/Positive Verbs in 2021")+
  theme_classic(base_family = "Century")+
  scale_y_continuous(limits = c(-1, 1))


grid.arrange(n21, v21, nrow = 1)
#######################################################################################################################################2022
all_nouns22<-full_join(POS_positive_nouns %>% filter(Time==2022)%>%count(lemma, sort=TRUE) %>% filter(n>10),
                       POS_negative_nouns %>%filter(Time==2022) %>% count(lemma, sort=TRUE) %>% filter(n>10),
                       by="lemma")

all_verbs22<-full_join(POS_positive_verbs %>%filter(Time==2022)%>% count(lemma, sort=TRUE) %>% filter(n>10),
                       POS_negative_verbs %>%filter(Time==2022)%>% count(lemma, sort=TRUE) %>% filter(n>10),
                       by="lemma")


all_nouns22 = rename(all_nouns22, "positive_count" = "n.x", "negative_count" = "n.y")
all_nouns22[is.na(all_nouns22$positive_count), "positive_count"]<- 0 
all_nouns22[is.na(all_nouns22$negative_count), "negative_count"]<- 0
all_nouns22$positive_count  <- all_nouns22$positive_count/sum(all_nouns22$positive_count)
all_nouns22$negative_count  <- all_nouns22$negative_count/sum(all_nouns22$negative_count)
all_nouns22$diff <- all_nouns22$positive_count-all_nouns22$negative_count


n22 <- all_nouns22 %>%
  mutate(word = reorder(lemma, positive_count)) %>%
  top_n(30, positive_count) %>%
  ggplot(aes(word, positive_count, fill = positive_count)) +
  geom_hline(yintercept = 0, color = "lightgray", linetype = "solid") +
  geom_col(width = 0.25) +
  labs(x = NULL, y = NULL, fill = NULL) +
  coord_flip() +
  scale_fill_gradient2(low = "red", mid = "orange", high = "darkgreen",limits = c(-1, 1), midpoint = 0) +
  ggtitle("Specific Negative/Positive Nouns in 2022")+
  theme_classic(base_family = "Century")+
  scale_y_continuous(limits = c(-1, 1))+
  theme(legend.position = "none") 

##########################################################VERBS

all_verbs22 = rename(all_verbs22, "positive_count" = "n.x", "negative_count" = "n.y")
all_verbs22[is.na(all_verbs22$positive_count), "positive_count"]<- 0
all_verbs22[is.na(all_verbs22$negative_count), "negative_count"]<-0
all_verbs22$positive_count  <- all_verbs22$positive_count/sum(all_verbs22$positive_count)
all_verbs22$negative_count  <- all_verbs22$negative_count/sum(all_verbs22$negative_count)
all_verbs22$diff <- all_verbs22$positive_count-all_verbs22$negative_count
v22 <- all_verbs22 %>%
  mutate(word = reorder(lemma, positive_count)) %>%
  top_n(30, positive_count) %>%
  ggplot(aes(word, positive_count, fill = positive_count)) +
  geom_hline(yintercept = 0, color = "lightgray", linetype = "solid") +
  geom_col(width = 0.2) +
  labs(x = NULL, y = NULL, fill = NULL) +
  coord_flip() +
  theme(text = element_text(size = 17),) +
  scale_fill_gradient2(low = "red", mid = "orange", high = "darkgreen",limits = c(-1, 1), midpoint = 0) +
  ggtitle("Specific Negative/Positive Verbs in 2022")+
  theme_classic(base_family = "Century")+
  scale_y_continuous(limits = c(-1, 1))


grid.arrange(n22, v22, nrow = 1)

#######################################################################################################################################2023
all_nouns23<-full_join(POS_positive_nouns %>% filter(Time==2023)%>%count(lemma, sort=TRUE) %>% filter(n>10),
                       POS_negative_nouns %>%filter(Time==2023) %>% count(lemma, sort=TRUE) %>% filter(n>10),
                       by="lemma")

all_verbs23<-full_join(POS_positive_verbs %>%filter(Time==2023)%>% count(lemma, sort=TRUE) %>% filter(n>10),
                       POS_negative_verbs %>%filter(Time==2023)%>% count(lemma, sort=TRUE) %>% filter(n>10),
                       by="lemma")


all_nouns23 = rename(all_nouns23, "positive_count" = "n.x", "negative_count" = "n.y")
all_nouns23[is.na(all_nouns23$positive_count), "positive_count"]<- 0 
all_nouns23[is.na(all_nouns23$negative_count), "negative_count"]<- 0
all_nouns23$positive_count  <- all_nouns23$positive_count/sum(all_nouns23$positive_count)
all_nouns23$negative_count  <- all_nouns23$negative_count/sum(all_nouns23$negative_count)
all_nouns23$diff <- all_nouns23$positive_count-all_nouns23$negative_count


n23 <- all_nouns23 %>%
  mutate(word = reorder(lemma, diff)) %>%
  top_n(30, diff) %>%
  ggplot(aes(word, diff, fill = diff)) +  
  geom_hline(yintercept = 0, color = "lightgray", linetype = "solid") +
  geom_col(width = 0.25) +
  labs(x = NULL, y = NULL, fill = NULL) +
  coord_flip() +
  scale_fill_gradient2(low = "red", mid = "orange", high = "darkgreen",limits = c(-1, 1), midpoint = 0) +
  ggtitle("Specific Negative/Positive Nouns in 2023")+
  theme_classic(base_family = "Century")+
  scale_y_continuous(limits = c(-1, 1))+
  theme(legend.position = "none") 

##########################################################VERBS

all_verbs23 = rename(all_verbs23, "positive_count" = "n.x", "negative_count" = "n.y")
all_verbs23[is.na(all_verbs23$positive_count), "positive_count"]<- 0
all_verbs23[is.na(all_verbs23$negative_count), "negative_count"]<-0
all_verbs23$positive_count  <- all_verbs23$positive_count/sum(all_verbs23$positive_count)
all_verbs23$negative_count  <- all_verbs23$negative_count/sum(all_verbs23$negative_count)
all_verbs23$diff <- all_verbs23$positive_count-all_verbs23$negative_count
v23 <- all_verbs23 %>%
  mutate(word = reorder(lemma, positive_count)) %>%
  top_n(30, positive_count) %>%
  ggplot(aes(word, positive_count, fill = positive_count)) +
  geom_hline(yintercept = 0, color = "lightgray", linetype = "solid") +
  geom_col(width = 0.2) +
  labs(x = NULL, y = NULL, fill = NULL) +
  coord_flip() +
  theme(text = element_text(size = 17),) +
  scale_fill_gradient2(low = "red", mid = "orange", high = "darkgreen",limits = c(-1, 1), midpoint = 0) +
  ggtitle("Specific Negative/Positive Verbs in 2023")+
  theme_classic(base_family = "Century")+
  scale_y_continuous(limits = c(-1, 1))



grid.arrange(n23, v23, nrow = 1)
######################################################################################  L D A

dtm <- CreateDtm(doc_vec = Application[, "Comments_NOSTOP_STEMMED"],
                 doc_names = Application[, "ID"])

term_count <- col_sums(as.matrix(dtm)) 
term_perc <- term_count / sum(term_count) * 100

### remove infrequent words
n<- nrow(dtm)
keep_cols <- term_count[term_count>0.01*n]
dtm<-dtm[,names(keep_cols)]

length(keep_cols)
names(keep_cols)

###Split into train and validation sample
set.seed(1)
train <- sample(1:n, round(n * 0.80))
valid <- (1:n)[-train]
dtm_train <- dtm[train,]
dtm_val <- dtm[-train,]


### Fit a Latent Dirichlet Allocation topic model using Gibbs sampling for different number of topics
res <- NULL
TMlist <- list()  
n_topics <- 10
n_topics_try<- seq(7,25,1)
for (i in 1:length(n_topics_try)){
  n_topics<- n_topics_try[i]
  print(n_topics)
  
  lda_results <- FitLdaModel(dtm = dtm_train,
                             k = n_topics, 
                             burnin = 200 + 10*n_topics,
                             iterations = 700 + 10*n_topics,
                             alpha = 0.1,beta = 0.05,
                             optimize_alpha = T,
                             calc_likelihood = T,
                             calc_coherence = T) 
  
  coh_train <- mean(CalcProbCoherence(phi = lda_results$phi, dtm = dtm_train, M = 5) )
  coh_val <- mean(CalcProbCoherence(phi = lda_results$phi, dtm = dtm_val, M = 5) )
  ll_train <- CalcLikelihood(dtm = dtm_train, 
                             phi = lda_results$phi, 
                             theta = lda_results$theta)/nrow(dtm_train)
  lda_results$theta_val <- predict(lda_results, dtm_val, method = "gibbs", iterations = 700 + 10*n_topics, burnin = 200 + 10*n_topics)
  ll_val <- CalcLikelihood(dtm = dtm_val, 
                           phi = lda_results$phi, 
                           theta = lda_results$theta_val)/nrow(dtm_val)
  res <- rbind(res, data.frame(n_topics=n_topics, ll_train = ll_train, ll_validation = ll_val, coh_train=coh_train, coh_val=coh_val))
  TMlist <- append(TMlist, c(lda_results))
  
  print(res)
}

color_paletteLDA <- brewer.pal(n = 2, name = "Set1")
# ## Plot likelihood
ll <- ggplot(res, aes(n_topics)) +                    
  geom_line(aes(y = ll_train, colour = "Train"), size = 2, show.legend = FALSE) + 
  geom_line(aes(y = ll_validation, colour = "Validation"), size = 2, show.legend = FALSE) + 
  geom_vline(xintercept = 11, color = "black", linetype = "dashed", size = 1) +
  labs(x = "Number of Topics", y = "Likelihood") +
  ggtitle("Determine the number of topics") +
  scale_colour_manual(values = color_paletteLDA) + 
  theme_minimal() +
  theme(
    axis.text = element_text(size = 15), 
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold")
  )
res[which.max(res$ll_validation),]

# ### How many topics do we need to select?
##Plot coherence
co <- ggplot(res, aes(n_topics)) +                    
  geom_line(aes(y = coh_train, colour = "Train"), size = 2) + 
  geom_line(aes(y = coh_val, colour = "Validation"), size = 2) + 
  geom_vline(xintercept = 11, color = "black", linetype = "dashed", size = 1) +
  labs(x = "Number of Topics", y = "Coherence", colour = "Sample") +
  scale_colour_manual(values = color_paletteLDA) + 
  theme_minimal() +
  theme(
    axis.text = element_text(size = 15), 
    axis.title = element_text(size = 14), 
    legend.text = element_text(size = 12), 
    legend.title = element_text(size = 14) 
  )
res[which.max(res$coh_val),]
# 
grid.arrange(ll, co, ncol = 2)


#AFTER OPTIMAZITION
alpha = .1
n_topics = 11#########CAHNGE
set.seed(1)
lda_results <- FitLdaModel(dtm = dtm_train,
                           k = n_topics, # number of topics
                           burnin = 200 + 10*n_topics,
                           iterations = 700 + 10*n_topics,
                           alpha = alpha, beta = 0.05,
                           optimize_alpha = T,
                           calc_likelihood = T,
                           calc_coherence = T)


model <- lda_results
model$prevalence <- colSums(model$theta) / sum(model$theta) * 100

plot(model$prevalence, model$alpha, xlab = "% topic prevalence", ylab = "alpha")

maxProb <- apply(lda_results$theta, 1, max)
hist(maxProb, main="Maximum topic probability", xlab="Maximum topic probability")


model$top_terms <- GetTopTerms(phi = model$phi, M = 12)
model$top_terms
SummarizeTopics(lda_results)

phi <- lda_results$phi
top_term_table <- NULL
n_topics = 11#########CAHNGE
set.seed(1)
for (j in 1:n_topics) {
  words <- t(phi[j,]) %>% order(decreasing=TRUE) %>% head(12) %>% phi[j,.]
  top_term_table <- rbind(top_term_table, data.frame(topic=j,probability=words , term = labels(words)) )
}

text_top_terms <- top_term_table %>%
  group_by(topic) %>%
  top_n(12, probability) %>%
  ungroup() %>%
  arrange(topic, -probability)
perplot <- 11#########CAHNGE
set.seed(1)
color_paletteX <- c(
  "#FF5733", "#33FF57", "#3357FF", "#FF33A1", "#33FFA1", "#A133CF", "#FFD700",
  "#8B0000", "#00CED1", "#9400D3", "#FF4500", "#2E8B57", "#4682B4", "#D2691E",
  "#FF1493", "#7CFC00", "#6A5ACD"
)

for (i in 1:ceiling(n_topics/perplot)) {
  p <- text_top_terms %>%
    filter(topic > (i-1)*perplot & topic <= (i*perplot)) %>% 
    mutate(term = reorder_within(term, probability, topic)) %>% 
    ggplot(aes(term, probability, fill = factor(topic))) + 
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip() +
    scale_x_reordered() +
    scale_fill_manual(values = color_paletteX) + 
    theme_classic(base_family = "Century") +
    labs(x = NULL, y = NULL) +
    ggtitle("Identified LDA Topics of Application3") +
    theme(plot.title = element_text(hjust = 0.5))
  show(p)
}

set.seed(1)
topic_distributions <- predict(lda_results, dtm, method = "gibbs", iterations = 700 + 10*n_topics, burnin = 200 + 10*n_topics)


most_likely_topics <- apply(topic_distributions, 1, which.max)

Application<- cbind(Application, Topic=topic_distributions)
Application$Most_Likely_Topic <- most_likely_topics
Application$Most_Likely_Topic<- as.factor(Application$Most_Likely_Topic)

######################################################## SUM

color_palette <- c(
  "#FF5733", "#33FF57", "#3357FF", "#FF33A1", "#33FFA1", "#A133CF", "#FFD700",
  "#8B0000", "#00CED1", "#9400D3", "#FF4500", "#2E8B57", "#4682B4", "#D2691E",
  "#FF1493", "#7CFC00", "#6A5ACD"
)

Application_Topic <- Application %>%
  group_by(Time, Most_Likely_Topic) %>%
  summarise(topic_count = n())

topics2018 <- Application_Topic %>%
  filter(Time == 2018)
topics2018 <- topics2018 %>%
  group_by(Time) %>%
  mutate(relative_count = topic_count / sum(topic_count)) %>%
  ungroup()
##########################################
topics2019 <- Application_Topic %>%
  filter(Time == 2019)
topics2019 <- topics2019 %>%
  group_by(Time) %>%
  mutate(relative_count = topic_count / sum(topic_count)) %>%
  ungroup()
##########################################
topics2020 <- Application_Topic %>%
  filter(Time == 2020)
topics2020 <- topics2020 %>%
  group_by(Time) %>%
  mutate(relative_count = topic_count / sum(topic_count)) %>%
  ungroup()
##########################################
topics2021 <- Application_Topic %>%
  filter(Time == 2021)
topics2021 <- topics2021 %>%
  group_by(Time) %>%
  mutate(relative_count = topic_count / sum(topic_count)) %>%
  ungroup()
##########################################
topics2022 <- Application_Topic %>%
  filter(Time == 2022)
topics2022 <- topics2022 %>%
  group_by(Time) %>%
  mutate(relative_count = topic_count / sum(topic_count)) %>%
  ungroup()
##########################################
topics2023 <- Application_Topic %>%
  filter(Time == 2023)
topics2023 <- topics2023 %>%
  group_by(Time) %>%
  mutate(relative_count = topic_count / sum(topic_count)) %>%
  ungroup()

TopicsAll<-rbind(topics2018,topics2019,topics2020,topics2021,topics2022,topics2023)
TopicsAll$Most_Likely_Topic <- as.factor(TopicsAll$Most_Likely_Topic)

NumberOfReviews<- TopicsAll %>%
  group_by(Most_Likely_Topic)%>%
  summarise(sum=sum(topic_count))
new_order <- c("9", "11", "3", "10", "5", "2", "7", "1", "8", "6", "4")
color_palette <- c(
  "#FF5733", "#33FF57", "#3357FF", "#FF33A1", "#33FFA1", "#A133FF", "#FFD700",
  "#8B0000", "#00CED1", "#9400D3", "#FF4500", "#2E8B57", "#4682B4", "#D2691E",
  "#FF1493", "#7CFC00", "#6A5ACD"
)
# Releveling the factor
TopicsAll$Most_Likely_Topic <- factor(TopicsAll$Most_Likely_Topic, levels = new_order)
# Create the relative stacked bar plot
ggplot(TopicsAll, aes(fill = Most_Likely_Topic, y = relative_count, x = factor(Time))) +
  scale_fill_manual(values = color_palette) +
  geom_bar(position = "fill", stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", relative_count * 100)), 
            position = position_fill(vjust = 0.5), size = 2, color = "black") +
  labs(x = NULL, y = NULL, fill = "Most Likely Topic") +
  ggtitle("Application 1") +
  theme_minimal()+
  theme_classic(base_family = "Century") 

TopicsSentiment<- TopicsAll
TopicsSentiment$Sentiment<- NA
TopicsSentiment$Sentiment <- ifelse(TopicsSentiment$Most_Likely_Topic == 9, 
                                    "Negative", 
                                    ifelse(TopicsSentiment$Most_Likely_Topic %in% c(11, 3, 10, 5, 2), 
                                           "Neutral", 
                                           "Positive"))
TopicsSentimentGrouped<- TopicsSentiment %>%
  group_by(Time,Sentiment) %>%
  summarise(sum=sum(relative_count))
ggplot(TopicsSentimentGrouped, aes(fill = Sentiment, y = sum, x = factor(Time))) +
  scale_fill_manual(values = c( "#FF5733", "#FFD700", "#33FF57" )) +
  geom_bar(position = "fill", stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", sum * 100)), 
            position = position_fill(vjust = 0.5), size = 2, color = "black") +
  labs(x = NULL, y = NULL, fill = "Most Likely Topic") +
  ggtitle("Application 1") +
  theme_minimal()+
  theme_classic(base_family = "Century") 

  
  
Application_Emotion <- Application %>%
  group_by(Time) %>%
  summarise(anger=sum(anger),anticipation=sum(anticipation),disgust=sum(disgust),
            fear=sum(fear),joy=sum(joy),sadness=sum(sadness),trust=sum(trust))
Application_Emotion<-na.omit(Application_Emotion)

Application_Emotion_long <- Application_Emotion %>%
  pivot_longer(cols = -Time, names_to = "emotion", values_to = "count")


Application_Emotion_long <- Application_Emotion_long %>%
  group_by(Time) %>%
  mutate(relative_value = count / sum(count)) %>%
  ungroup()

color_paletteE <- c(
  "#FF5733", "#33FF57", "#3357FF", "#FF33A1", "#33FFA1", "#A133CF", "#FFD700",
  "#8B0000", "#00CED1", "#9400D3", "#FF4500", "#2E8B57", "#4682B4", "#D2691E",
  "#FF1493", "#7CFC00", "#6A5ACD"
)
ggplot(Application_Emotion_long, aes(fill = emotion, y = relative_value, x = factor(Time))) +
  scale_fill_manual(values = color_paletteE) +
  geom_bar(position = "fill", stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", relative_value * 100)), 
            position = position_fill(vjust = 0.5), size = 5, color = "black") +
  labs(x = NULL, y = NULL, fill = "Emotion") +
  ggtitle("Application 1") +
  theme_minimal()+
  theme_classic(base_family = "Century") 
############
NumberOfReviewsperTopic<- Application %>%
  group_by(Most_Likely_Topic)%>%
  summarise(n=n())


Application$Target<- Application$Star_rating>3
colnames(modeldata)

modeldata<- Application[,c(64,63,6,8:51)]
modeldata <- modeldata %>%
  mutate_if(is.logical, as.factor)
modeldata$Most_Likely_Topic<- as.factor(modeldata$Most_Likely_Topic)
modeldata[,c(3,40:47)]<-scale(modeldata[,c(3,40:47)])
modeldata<-na.omit(modeldata)
table(modeldata$Target)
colnames(modeldata)
set.seed(123)
n <- nrow(modeldata)
train_index <- sample(1:n, 0.8 * n)

train_data <- modeldata[train_index, ]
test_data <- modeldata[-train_index, ]

#
X_train <- as.matrix(train_data[, -1])  
y_train <- train_data$Target      
X_test <- as.matrix(test_data[, -1])   
y_test <- test_data$Target        


set.seed(1)
lasso_model <- cv.glmnet(X_train, y_train, alpha = 1, family = "binomial", nfolds = 20)  # Logistic regression
plot(lasso_model)
best_lambda <- lasso_model$lambda.min
set.seed(1)
lasso_model_best <- glmnet(X_train, y_train, alpha = 1, lambda = best_lambda, family = "binomial")
predictions_train <- predict(lasso_model_best, newx = X_train, type = "response")  # Predictions on training set
predictions_test <- predict(lasso_model_best, newx = X_test, type = "response")  

# Calculate hit rate (accuracy) for training and test sets
hit_rate_train <- mean(ifelse(predictions_train > 0.5, TRUE, FALSE) == y_train)
hit_rate_test <- mean(ifelse(predictions_test > 0.5, TRUE, FALSE) == y_test)

# Calculate log likelihood for training and test sets
log_likelihood_train <- sum(log(ifelse(y_train == 1, predictions_train, 1 - predictions_train)))
log_likelihood_test <- sum(log(ifelse(y_test == 1, predictions_test, 1 - predictions_test)))

hit_rate_train
hit_rate_test
log_likelihood_train
log_likelihood_test
lasso_model_best$beta
#GET THE RESULT OF LASSO FOR NEW FEATURES
lasso_model_best$beta
coefficients <- lasso_model_best$beta@Dimnames[[1]]
coefficients_index <- lasso_model_best$beta@i+1
new_features<- coefficients[coefficients_index]

#REDO THE REGRESSION WITH THE NEW FEATURES
features<- modeldata[,-1]
target<- modeldata[,1]
new_features<- features[,coefficients_index]


normalized_new_data <- cbind(Star_rating = target, new_features)

set.seed(1)
n <- nrow(normalized_new_data)
train_index <- sample(1:n, 0.8 * n)

train_new_data <- normalized_new_data[train_index, ]
test_new_data <- normalized_new_data[-train_index, ]

#
X_new_train <- as.matrix(train_new_data[, -1])  
y_new_train <- train_new_data$Star_rating      
X_new_test <- as.matrix(test_new_data[, -1])   
y_new_test <- test_new_data$Star_rating        

set.seed(1)
logit_model <- glm(Star_rating ~ ., data = train_new_data, family = binomial)

train_predictions <- predict(logit_model, newdata = train_new_data, type = "response")
test_predictions <- predict(logit_model, newdata = test_new_data, type = "response")

train_accuracy <- mean((train_predictions >= 0.5) == (train_new_data$Star_rating == 1))
test_accuracy <- mean((test_predictions >= 0.5) == (test_new_data$Star_rating == 1))

# Calculate log likelihood for training and test sets
train_log_likelihood <- sum(log(ifelse(train_new_data$Star_rating == 1, train_predictions, 1 - train_predictions)))
test_log_likelihood <- sum(log(ifelse(test_new_data$Star_rating == 1, test_predictions, 1 - test_predictions)))

train_accuracy
test_accuracy
train_log_likelihood
test_log_likelihood
summary_data_cv <-summary(logit_model)

significant_coefficients <- summary_data_cv$coefficients[summary_data_cv$coefficients[, "Pr(>|z|)"] < 0.1, ]
significant_coefficients<- as.data.frame(round(significant_coefficients,3))

for (i in 1:nrow(significant_coefficients)) {
  if(significant_coefficients$Estimate[i] < 0){
    significant_coefficients$odds_positive[i] <- ((1-(exp(significant_coefficients$Estimate[i])))*100)} else {
      significant_coefficients$odds_positive[i] <- (exp(significant_coefficients$Estimate[i])-1)*100}}
significant_coefficients


((1-(exp(significant_coefficients$Estimate[2])))*100)
###################################################################################

Significant_words <- Application %>%
  group_by(Time) %>%
  summarise(
    easi_use = sum(`easi use`, na.rm = TRUE),
    number_trip = sum(`number trip`, na.rm = TRUE),
    keep_track = sum(`keep track`, na.rm = TRUE),
    not_work = sum(`not work`, na.rm = TRUE),
    log_trip = sum(`log trip`, na.rm = TRUE),
    tax_time = sum(`tax time`, na.rm = TRUE),
    not_even = sum(`not even`, na.rm = TRUE),
    track_work = sum(`track work`, na.rm = TRUE),
    log_book = sum(`log book`, na.rm = TRUE)
  )

Significant_words <- merge(Significant_words, Num_of_Reviews)

Significant_words <- Significant_words %>%
  mutate(
    easi_use_rel = easi_use / total_count,
    number_trip_rel = number_trip / total_count,
    keep_track_rel = keep_track / total_count,
    not_work_rel = not_work / total_count,
    log_trip_rel = log_trip / total_count,
    tax_time_rel = tax_time / total_count,
    not_even_rel = not_even / total_count,
    track_work_rel = track_work / total_count,
    log_book_rel = log_book / total_count
  )


Significant_Negative_BI <- Significant_words[,c(1,13,15,18)] %>%
  pivot_longer(cols = -Time, names_to = "Bigram", values_to = "Value")
Significant_Negative_BI<- na.omit(Significant_Negative_BI)
color_palette2 <- brewer.pal(n = 3, name = "Set3")
# Create the stacked bar plot
neg_bigram<-ggplot(Significant_Negative_BI, aes(x = factor(Time), y = Value, fill = Bigram)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = color_palette2, labels = c("not even", "not work", "number trip")) +
  labs(x = NULL, y = NULL, fill = "Bigram:") +
  ggtitle("Distribution of Bigrams with Negative Effect") +
  theme_minimal() +
  theme_classic(base_family = "Century") +
  ylim(0, 0.4)

Significant_Positive_BI <- Significant_words[,c(1,12,14,16,17,19,20)] %>%
  pivot_longer(cols = -Time, names_to = "Bigram", values_to = "Value")
Significant_Positive_BI<- na.omit(Significant_Positive_BI)
color_palette3 <- brewer.pal(n = 6, name = "Set2")
# Create the stacked bar plot

pos_bigram<-ggplot(Significant_Positive_BI, aes(x = factor(Time), y = Value, fill = Bigram)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = color_palette3, labels = c("easi use", "keep track", "log book","log book","tax time","track work")) +
  labs(x = NULL, y = NULL, fill = "Bigram:") +
  ggtitle("Distribution of Bigrams with Positive Effect") +
  theme_minimal() +
  theme_classic(base_family = "Century") +
  ylim(0, 0.4)

avgSentScoreDriversnote<- Application %>%
  group_by(Time)%>%
  summarise( mean = mean(Sentiment))

sentiment_line_plotDriversnote <- ggplot(avgSentScoreDriversnote, aes(x = Time, y = mean, color = "Avg .Sentiment Score")) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = round(mean, 2)), vjust = -0.5) +
  labs(x = NULL, y = NULL, title = "Average Sentiment Score Over Time", color = NULL) +
  scale_color_manual(values = color_palette3[5]) +  
  theme_minimal() +
  scale_x_continuous(breaks = unique(avgSentScoreDriversnote$Time))+
  theme_classic(base_family = "Century") +
  ylim(0, 0.6)

grid.arrange(pos_bigram,neg_bigram,sentiment_line_plot,nrow=3)


TopicsPositive<-TopicsAll[TopicsAll$Most_Likely_Topic %in% c(2,5,6),]
color_palette4 <- brewer.pal(n = 3, name = "Set2")
ggplot(TopicsPositive, aes(fill = Most_Likely_Topic, y = relative_count, x = factor(Time))) +
  scale_fill_manual(values = color_palette4) +
  geom_bar(position = "stack", stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", relative_count * 100)), 
            position = position_stack(vjust = 0.5), size = 2, color = "black") +
  labs(x = NULL, y = NULL, fill = "Positive Topics:") +
  ggtitle("Portion of Significant Positive Topics") +
  theme_minimal()+
  theme_classic(base_family = "Century")+
  ylim(0,1)

######################################################################################
#Sentiment Topic

Sentences <- Application[,c(1,3,4,5,63)] %>%
  unnest_tokens(Comments, Comments, token = "sentences")
Sentences$Sentiment <-sentiment(Sentences$Comments_NOSTOP, lexicon::hash_sentiment_huliu)$sentiment

TopicSentiment<- Sentences[,c(2,5,6)]
str(TopicSentiment)
TopicSentiment$Most_Likely_Topic <- as.factor(TopicSentiment$Most_Likely_Topic)

##################################################################
###ANOVA
anova_result <- aov(Sentiment ~ Most_Likely_Topic, data = TopicSentiment)
summary(anova_result)
tukey_result <- TukeyHSD(anova_result)
tukey_result

TopicSentiment$Target<- TopicSentiment$Star_rating>3
TopicSentiment$Star_rating<- NULL
table(TopicSentiment$Target)
TopicSentiment<-na.omit(TopicSentiment)

set.seed(123)
n <- nrow(TopicSentiment)
train_index <- sample(1:n, 0.8 * n)

train_data <- TopicSentiment[train_index, ]
test_data <- TopicSentiment[-train_index, ]

#
X_train <- as.matrix(train_data[, -3])  
y_train <- train_data$Target      
X_test <- as.matrix(test_data[, -3])   
y_test <- test_data$Target        

set.seed(1)

logit_model <- glm(Target ~ Sentiment * Most_Likely_Topic, data = train_data, family = binomial)
predictions_train <- predict(logit_model, newx = X_train, type = "response")  # Predictions on training set
predictions_test <- predict(logit_model, newx = X_test, type = "response")  
summary(logit_model)

#############################################################
TopicSentimentAggregate<- TopicSentiment%>%
  group_by(Most_Likely_Topic)%>%
  summarise(mean=mean(Sentiment), sd = sd(Sentiment), n=n())



###############################################################
#RANDOM FOREST

modeldataRF<- Application[,c(64,63,6,8:51)]
modeldataRF <- modeldataRF %>%
  mutate_if(is.logical, as.factor)
modeldataRF$Most_Likely_Topic<- as.factor(modeldataRF$Most_Likely_Topic)
modeldataRF<-na.omit(modeldataRF)

for (i in 4:39) {
  colnames(modeldataRF)[i] <- paste0("V", i)
}

set.seed(123)
n <- nrow(modeldataRF)
train_index <- sample(1:n, 0.8 * n)

train_dataRF <- modeldataRF[train_index, ]
test_dataRF <- modeldataRF[-train_index, ]


random_model <- NULL
results_mtry <- data.frame("mtry" = 1:7)

set.seed(1)
for(i in 4:10) {
  random_model <- randomForest(Target~., data = train_dataRF, mtry = i)
  results_mtry[i,"mtry"] <- i
  results_mtry[i,"OOB_error"] <- random_model$err.rate[nrow(random_model$err.rate),"OOB"]
  print(i)
}

rf_model <- randomForest(Target ~., data = train_dataRF, mtry = 5)
plot(rf_model)

test_dataRF$prediction <- predict(rf_model, test_dataRF)
cm_rf <- xtabs(~prediction + Target, data = test_dataRF)
print(cm_rf)

confusionMatrix(test_dataRF$prediction, test_dataRF$Target)
accuracy <- (cm_rf["TRUE","TRUE"] + cm_rf["FALSE","FALSE"])/nrow(test_dataRF)
recall <- cm_rf["FALSE","FALSE"]/(cm_rf["FALSE","FALSE"] + cm_rf["TRUE","FALSE"])

specificity <- cm_rf["TRUE","TRUE"]/(cm_rf["TRUE","TRUE"] + cm_rf["FALSE","TRUE"])

test <- glm(Target ~., data = train_dataRF, family = "binomial")
test_dataRF$prediction_test <- ifelse(predict(test, test_dataRF, type = "response") > 0.5, TRUE, FALSE)

cm_log <- xtabs(~prediction_test + Target, data = test_dataRF)
print(cm_rf)

accuracy_log <- (cm_log["TRUE","TRUE"] + cm_log["FALSE","FALSE"])/nrow(test_dataRF)

recall_log <- cm_log["FALSE","FALSE"]/(cm_log["FALSE","FALSE"] + cm_log["TRUE","FALSE"])

specificity_log <- cm_log["TRUE","TRUE"]/(cm_log["TRUE","TRUE"] + cm_log["FALSE","TRUE"])


summary(test)
varImpPlot(rf_model)

importance_values <- importance(rf_model)
importance_df <- data.frame(Feature = rownames(importance_values), 
                            MeanDecreaseGini = importance_values[, "MeanDecreaseGini"])

top5_importance_df <- importance_df %>%
  arrange(desc(MeanDecreaseGini)) %>%
  head(10)

# Plot the top 5 features
p <- ggplot(top5_importance_df, aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Variable Importance by Mean Decrease Gini", 
       x = "Features", 
       y = "Mean Decrease Gini") +
  theme_minimal() +
  theme_classic(base_family = "Century")
p


pdp_results_topics <- partial(rf_model,pred.var = "Most_Likely_Topic", prob = TRUE, which.class = 2L, type = "classification")
pdp_df <- as.data.frame(pdp_results_topics)
ggplot(pdp_df, aes(x = `Most_Likely_Topic`, y = `yhat`)) +               
  geom_point(aes(color = `Most_Likely_Topic`),  
             size = 4) +
  geom_text(aes(label = paste0(round(yhat * 100, 1), " %")),             
            vjust = -0.9,                             
            size = 4,
            family="Century") +
  scale_color_manual(values = color_palette) + 
  scale_y_continuous(limits = c(0, 1),                
                     breaks = seq(0, 1, by = 0.1)) +  
  labs(title = "Predicted Probability of Positive Review",
       x = "Topics",
       y = "Probability") +
  theme_minimal() +
  theme_classic(base_family = "Century")

pdp_results_sentiment <- partial(rf_model,pred.var = "Sentiment", prob = TRUE, which.class = 2L, type = "classification")
plotPartial(pdp_results_sentiment, ylim = c(0,1), ylab = "Predicted probability of positive review", xlab = "Topics", main = "Topics")
ggplot(pdp_results_sentiment, aes(x = Sentiment, y = yhat)) +
  geom_line(color = color_palette3[5], size = 2) +  
  geom_vline(xintercept = 0, color = "grey") +  
  labs(x = "Sentiment score", y = "Probability") + 
  ggtitle("Predicted probability of positive review") +  
  theme_minimal()+
  theme_classic(base_family = "Century")

pdp_results_v4 <- partial(rf_model,pred.var = "V4", prob = TRUE, which.class = 2L, type = "classification")
plotPartial(pdp_results_v4, ylim = c(0,1), ylab = "Predicted probability of positive review", xlab = "Topics", main = "Topics")

pdp_results_anger <- partial(rf_model,pred.var = "anger", prob = TRUE, which.class = 2L, type = "classification")
plotPartial(pdp_results_anger, ylim = c(0,1), ylab = "Predicted probability of positive review", xlab = "Topics", main = "Topics")

pdp_results_disgust <- partial(rf_model,pred.var = "disgust", prob = TRUE, which.class = 2L, type = "classification")
plotPartial(pdp_results_disgust, ylim = c(0,1), ylab = "Predicted probability of positive review", xlab = "Topics", main = "Topics")
