# Setting work directory
setwd("~/EON")

#Required Packages

library(stm)
library(tm)
library(lubridate)
library(wordcloud)
#library(tidyr)
library(topicmodels)
library(tidytext)
library(tidyverse)
#library(dplyr)
#install.packages("ggthemes")
library(ggthemes)
library(scales)

#install.packages("stminsights")
#library(stminsights)

#Reading Dataset
eon_data = read.csv('eon_stm.csv',colClasses = c("NULL",NA,NA,NA))
str(eon_data)

identifier_eon <- read.csv('eon_stm.csv',colClasses = c(NA,"NULL","NULL","NULL"))
str(identifier_eon)

# changing into factors
eon_data$Month_Of_Occurrence <- factor(month(as.Date(eon_data$Date_Of_Occurrence),label=TRUE),ordered=FALSE) 

eon_data$Aircraft_Type <- as.factor(eon_data$Aircraft_Type)

#PrepDocuments is a helpful function to perform some manipulations like removing words based on thresholds 
#without compromising the indexes 
processed_eon <- textProcessor(eon_data$Remarks,stem = FALSE,onlycharacter = TRUE,wordLengths= c(4,Inf),
                               customstopwords= c('reported','aircraft','declared','pilot','aborted','issue',
                                                  'notified','acft','departure','departed',
                                                  'diverted','flight','feet','reporting','final','bound','unknown',
                                                  'requested','airport','authorization','arrival',
                                                  "without", "landed", "taxi", "taxiing", "onto", "contact", "contacted", "none", "cancelled",
                                                  "wout", "year", "called", "advised", "assigned", "continued", "areaentered", "oclock",
                                                  "instructed","Made", "phone", "later" , "descended", "found", "plane" , "informed", "made" , "appeared" ,
                                                  "proceeded","needed", "informed", "stated", "return", "went", "going","near" ),metadata=eon_data)

out<- prepDocuments(processed_eon$documents, processed_eon$vocab, processed_eon$meta,lower.thresh = 0)

docs_eon <- out$documents
vocab_eon <- out$vocab
meta_eon <- out$meta

## For finding the optimal Number of Topics

kResult <- searchK(out$documents, out$vocab, K=c(15:30), prevalence=~Aircraft_Type+Month_Of_Occurrence,
                   data=out$meta,gamma.prior='L1', max.em.its=10, seed=1111)

plot(kResult)

stm_eon_25 <- stm(out$documents, out$vocab, K=25, prevalence=~Aircraft_Type+Month_Of_Occurrence, 
                       max.em.its=10, data=out$meta, init.type="Spectral",gamma.prior='L1', seed=1111)

stm_eon_20 <- stm(out$documents, out$vocab, K=20, prevalence=~Aircraft_Type+Month_Of_Occurrence, 
                  max.em.its=10, data=out$meta, init.type="Spectral",gamma.prior='L1', seed=1111)


stm_eon_30 <- stm(out$documents, out$vocab, K=30, prevalence=~Aircraft_Type+Month_Of_Occurrence, 
                  max.em.its=10, data=out$meta, init.type="Spectral",gamma.prior='L1', seed=1111)



stm_eon_29 <- stm(out$documents, out$vocab, K=29, prevalence=~Aircraft_Type+Month_Of_Occurrence, 
                  max.em.its=10, data=out$meta, init.type="Spectral",gamma.prior='L1', seed=1111)


# plot can be used to visualize the topic distribution 
plot(stm_eon_29,n=7)

#topic visualization

topic_model <- stm_eon_29 
topic_model

td_beta <- tidy(topic_model)
td_beta

td_gamma <- tidy(topic_model, matrix = "gamma")
td_gamma

top_terms <- td_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", "))

gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))
gamma_terms %>%
  top_n(29, gamma) %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE,fill = "steelblue4") +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 4,
            family = "IBMPlexSans") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.20),
                     labels = percent_format())+
  #scale_fill_brewer(fill = "goldenrod2")+
  theme_tufte(base_family = "IBMPlexSans", ticks = FALSE) +
  theme(plot.title = element_text(size = 10,
                                  family="IBMPlexSans-Bold"),
        plot.subtitle = element_text(size = 8)) +
  labs(x = "Topics", y = "Proportion",
       title = "29 Identified Topics in the EON Dataset")

# The plot.STM function with "hist" as argument in order to visualize the estimates of document-topic proportions

plot.STM(stm_eon_29, type="hist")

topicQuality(model=stm_eon_29, documents=out$documents)

cloud_eon <- cloud(stm_eon_29)

#labelTopics gives us more detailed insights on the popular words in each topic. 
topics_eon <- labelTopics(stm_eon_29)
topics_eon

# creating new column with Dominant Topic Keywords based on Score metric


for (i in 1:dim(topics_eon$score)[1]){
  
  a=c()
  
  for (j in 1:dim(topics_eon$score)[2]){
    
    a = append(a,topics_eon$score[i,j])
    
  }
  
  a = paste(as.character(a), collapse=",")
  assign(paste("Topic", i, sep = ""), a)
  
}

#Making a data table of topic proportions
tab_eon <- make.dt(model=stm_eon_29, meta = eon_data)

str(tab_eon)

#Assigning each document with its dominant topic
tab_eon[, Dominant_Topic_eon := colnames(.SD)[max.col(.SD, ties.method="first")], .SDcols = c("Topic1", "Topic2","Topic3","Topic4","Topic5","Topic6","Topic7","Topic8","Topic9","Topic10","Topic11","Topic12","Topic13","Topic14","Topic15",'Topic16','Topic17','Topic18','Topic19','Topic20','Topic21','Topic22','Topic23','Topic24','Topic25',
                                                                                              "Topic26","Topic27","Topic28","Topic29")]
#converting into a data frame
tab_eon <- as.data.frame(tab_eon)

#making a list of all the topics

topics = list(Topic1,Topic2,Topic3,Topic4,Topic5,Topic6,Topic7,Topic8,Topic9,Topic10,Topic11,Topic12,Topic13,Topic14,Topic15,Topic16,Topic17,Topic18,Topic19,Topic20,Topic21,Topic22,Topic23,Topic24,Topic25,
              Topic26,Topic27,Topic28,Topic29)

#keywords for each topic

for (i in 1:dim(topics_eon$score)[1]){
  
  tab_eon$Dominant_Topic_eon[which(tab_eon$Dominant_Topic_eon == paste('Topic',i,sep=''))] = topics[[i]]
  
}

str(tab_eon)
#Making a subset of the original data frame to contain only dominant topic keywords.

final_eon_topic <- subset(tab_eon,select=c('Dominant_Topic_eon'))

# colnames(final_eon_topic)[colnames(final_eon_topic) == 'Event.Type'] <- 'Event Type'

#Adding a new column unique identifier to the final_aid_topic file from original aid_data
final_eon_topic$Unique_Identifier <- identifier_eon$Unique_Identifier

str(final_eon_topic)

#importing into CSV

write.csv(final_eon_topic,'final_eon_topic.csv',row.names = FALSE)



# Estimate effect
# The function to extract the relationship and associated uncertainty on all the topics of the model is 
# estimateEffect, which basically estimates a regression with documents as units, the outcome is the proportion
# of each document about a topic in an STM model and the covariates are document-meta data

prep <- estimateEffect(1:29 ~Month_Of_Occurrence, stm_eon_29, meta=out$meta, 
                       uncertainty="Global")

save.image('EON_Topic.RData')

# STM insights Dashboard
run_stminsights()


# Comparing exclusivity and semantic coherence
suppressWarnings(library(ggplot2))
#install.packages("htmlwidgets")
suppressWarnings(library(htmlwidgets))


#creating dataframes for each model with their respective semantic coherence and exclusivity scores

M25ExSem<-as.data.frame(cbind(c(1:25),exclusivity(stm_eon_25), semanticCoherence(model=stm_eon_25, docs_eon), "Mod25"))
M20ExSem<-as.data.frame(cbind(c(1:20),exclusivity(stm_eon_20), semanticCoherence(model=stm_eon_20, docs_eon), "Mod20"))
M29ExSem<-as.data.frame(cbind(c(1:29),exclusivity(stm_eon_29), semanticCoherence(model=stm_eon_29, docs_eon), "Mod29"))
M30ExSem<-as.data.frame(cbind(c(1:30),exclusivity(stm_eon_30), semanticCoherence(model=stm_eon_30, docs_eon), "Mod30"))

# combining the rows of all three dataframes
ModsExSem<-rbind(M20ExSem, M25ExSem,M29ExSem, M30ExSem)

#renaming column names for the above dataframe
colnames(ModsExSem)<-c("K","Exclusivity", "SemanticCoherence", "Model")

#converting SemanticCoherence and Exclusivity of the dataframe to numeric datatype
ModsExSem$Exclusivity<-as.numeric(as.character(ModsExSem$Exclusivity))
ModsExSem$SemanticCoherence<-as.numeric(as.character(ModsExSem$SemanticCoherence))

options(repr.plot.width=7, repr.plot.height=6, repr.plot.res=100)

dev.off()

# Comparing exclusivity and semantic coherence using ggplot
plotexcoer<-ggplot(ModsExSem, aes(SemanticCoherence, Exclusivity, color = Model))+geom_point(size = 2, alpha = 0.7) + 
  geom_text(aes(label=K), nudge_y=.04)+
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence")
plotexcoer

#Highest word probabilities for each topic
suppressWarnings(library(dplyr))
#suppressWarnings(library(drlib))#drlib is available on github and needs devtools to be installed

td_beta <- tidytext::tidy(stm_eon_29)

options(repr.plot.width=7, repr.plot.height=8, repr.plot.res=100)

td_beta %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = "Proportion", y = "Topic Words",
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")

# Word probabilities for Topic 1
betaT1<-td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%filter(topic=="Topic 1")#beta values for topic 1

betaplotT1<-ggplot(betaT1[betaT1$beta>0.003,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity")+coord_flip()+labs(x ="Terms", y = "Probability",
                                                                                  title = "Word probabilities for Topic 1")#plot word probabilities higher than 0.003 for topic 1

options(repr.plot.width=9, repr.plot.height=10, repr.plot.res=100)
betaplotT1










