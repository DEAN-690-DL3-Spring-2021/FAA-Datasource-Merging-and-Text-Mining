#setting work directory
setwd("~/SDR")
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
library(RColorBrewer)

#install.packages("stminsights")
#library(stminsights)

#Reading Dataset
sdr_data = read.csv('sdr_stm.csv',colClasses = c("NULL",NA,NA,NA,NA))
str(sdr_data)

identifier_sdr <- read.csv('sdr_stm.csv',colClasses = c(NA,"NULL","NULL","NULL","NULL"))
str(identifier_sdr)

# changing into factors
sdr_data$Month_Of_Occurrence <- factor(month(as.Date(sdr_data$Date_Of_Occurrence),label=TRUE),ordered=FALSE) 

sdr_data$Stage_Of_Operation <- as.factor(sdr_data$Stage_Of_Operation)


## textProcessor prepares the corpus, representing the documents as lists containting 
#word indicese and associated word counts, the vocab character vector associated with 
#the word indices and a metadata matrix containing the covariates. As we can remove some custom words, 
#here we opt to remove "aircraft" , "inspector" and so on from the corpus 

processed_sdr <- textProcessor(sdr_data$Remarks,stem = FALSE,onlycharacter = TRUE, wordLengths= c(4,Inf),
                               customstopwords=c('pilot','report','aircraft','classified','major','per','yes','performed','satisfactory','operational','check',
                                                 'accomplish','accomplished','issued','provide','instruction',
                                                 'scheduled','inspection','found'), metadata=sdr_data)

#PrepDocuments is a helpful function to perform some manipulations like removing words based on thresholds 
#without compromising the indexes 

out <- prepDocuments(processed_sdr$documents, processed_sdr$vocab, processed_sdr$meta,lower.thresh = 0)

docs_sdr <- out$documents
vocab_sdr <- out$vocab
meta_sdr <- out$meta

# For finding the optimal Number of Topics

kResult <- searchK(out$documents, out$vocab, K=c(25:45), prevalence=~Aircraft_Make+Stage_Of_Operation+Month_Of_Occurrence,
                   data=out$meta,gamma.prior='L1', max.em.its=10, seed=1111)

plot(kResult)

stm_sdr_30 <- stm(out$documents, out$vocab, K=30, prevalence=~Aircraft_Make+Stage_Of_Operation+Month_Of_Occurrence,
                             max.em.its=10, data=out$meta, init.type="Spectral",gamma.prior='L1', seed=1111)

stm_sdr_35 <- stm(out$documents, out$vocab, K=35, prevalence=~Aircraft_Make+Stage_Of_Operation+Month_Of_Occurrence,
                  max.em.its=10, data=out$meta, init.type="Spectral",gamma.prior='L1', seed=1111)

stm_sdr_44 <- stm(out$documents, out$vocab, K=44, prevalence=~Aircraft_Make+Stage_Of_Operation+Month_Of_Occurrence,
                  max.em.its=10, data=out$meta, init.type="Spectral",gamma.prior='L1', seed=1111)


# plot can be used to visualize the topic distribution 
plot(stm_sdr_44,n=5,topics =c(1,30))

#topic visualization

topic_model <- stm_sdr_44 
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
  top_n(30, gamma) %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE,fill = "steelblue4") +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 4,
            family = "IBMPlexSans") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.15),
                     labels = percent_format())+
  #scale_fill_brewer(fill = "grey")+
  theme_tufte(base_family = "IBMPlexSans", ticks = FALSE) +
  theme(plot.title = element_text(size = 12,
                                  family="IBMPlexSans-Bold"),
        plot.subtitle = element_text(size = 8)) +
  labs(x = "Topics", y = "Proportion",
       title = "Top 30 Topics in the SDR Dataset")


# The plot.STM function with "hist" as argument in order to visualize the estimates of document-topic proportions

plot(stm_sdr_44, type="hist")

cloud_sdr <- cloud(stm_sdr_44)


#labelTopics gives us more detailed insights on the popular words in each topic.
topics_sdr <- labelTopics(stm_sdr_44)

topics_sdr

topics_sdr$score

topicQuality(model=stm_sdr_44, documents=out$documents)

# creating new column with Dominant Topic Keywords based on Frex metric
for (i in 1:dim(topics_sdr$score)[1]){
  
  a=c()
  
  for (j in 1:dim(topics_sdr$score)[2]){
    
    a = append(a,topics_sdr$score[i,j])
    
  }
  
  a = paste(as.character(a), collapse=",")
  assign(paste("Topic", i, sep = ""), a)
  
}

#Making a data table of topic proportions
tab_sdr <- make.dt(model=stm_sdr_44, meta = sdr_data)

str(tab_sdr)

#Assigning each document with its dominant topic
tab_sdr[, Dominant_Topic_sdr := colnames(.SD)[max.col(.SD, ties.method="first")], .SDcols = c("Topic1", "Topic2","Topic3","Topic4","Topic5","Topic6","Topic7","Topic8","Topic9","Topic10","Topic11","Topic12","Topic13","Topic14","Topic15",'Topic16','Topic17','Topic18','Topic19','Topic20','Topic21','Topic22','Topic23','Topic24','Topic25','Topic26','Topic27','Topic28','Topic29','Topic30',
                                                                                              "Topic31","Topic32","Topic33","Topic34","Topic35","Topic36","Topic37","Topic38","Topic39","Topic40","Topic41","Topic42","Topic43","Topic44")]
#converting into a data frame
tab_sdr <- as.data.frame(tab_sdr)

#making a list of all the topics
topics = list(Topic1,Topic2,Topic3,Topic4,Topic5,Topic6,Topic7,Topic8,Topic9,Topic10,Topic11,Topic12,Topic13,Topic14,Topic15,Topic16,Topic17,Topic18,Topic19,Topic20,Topic21,Topic22,Topic23,Topic24,Topic25,Topic26,Topic27,Topic28,Topic29,Topic30,
              Topic31,Topic32,Topic33,Topic34,Topic35,Topic36,Topic37,Topic38,Topic39,Topic40,Topic41,Topic42,Topic43,Topic44)

#keywords for each topic
for (i in 1:dim(topics_sdr$score)[1]){
  
  tab_sdr$Dominant_Topic_sdr[which(tab_sdr$Dominant_Topic_sdr == paste('Topic',i,sep=''))] = topics[[i]]
  
}

str(tab_sdr)

#Making a subset of the original data frame to contain only dominant topic keywords.
final_sdr_topic <- subset(tab_sdr,select=c('Dominant_Topic_sdr'))

#Adding a new column unique identifier to the final_aid_topic file from original sdr_data
final_sdr_topic$Unique_Identifier <- identifier_sdr$Unique_Identifier

str(final_sdr_topic)

#importing into CSV
write.csv(final_sdr_topic,'final_sdr_topic.csv',row.names = FALSE)

# Estimate effect
# The function to extract the relationship and associated uncertainty on all the topics of the model is 
# estimateEffect, which basically estimates a regression with documents as units, the outcome is the proportion
# of each document about a topic in an STM model and the covariates are document-meta data


prep <- estimateEffect(1:42 ~Aircraft_Make+Stage_Of_Operation+Month_Of_Occurrence, stm_sdr_44, meta=out$meta, 
                       uncertainty="Global")

save.image('SDR_Topic.RData')

# STM insights Dashboard
run_stminsights()


# Comparing exclusivity and semantic coherence
suppressWarnings(library(ggplot2))
#install.packages("htmlwidgets")
suppressWarnings(library(htmlwidgets))

#creating dataframes for each model with their respective semantic coherence and exclusivity scores
M30ExSem<-as.data.frame(cbind(c(1:30),exclusivity(stm_sdr_30), semanticCoherence(model=stm_sdr_30, docs_sdr), "Mod30"))
M35ExSem<-as.data.frame(cbind(c(1:35),exclusivity(stm_sdr_35), semanticCoherence(model=stm_sdr_35, docs_sdr), "Mod35"))
M44ExSem<-as.data.frame(cbind(c(1:44),exclusivity(stm_sdr_44), semanticCoherence(model=stm_sdr_44, docs_sdr), "Mod44"))

# combining the rows of all three dataframes
ModsExSem<-rbind(M30ExSem, M35ExSem, M44ExSem)

#renaming column names for the above dataframe
colnames(ModsExSem)<-c("K","Exclusivity", "SemanticCoherence", "Model")

#converting SemanticCoherence and Exclusivity of the dataframe to numeric datatype
ModsExSem$Exclusivity<-as.numeric(as.character(ModsExSem$Exclusivity))
ModsExSem$SemanticCoherence<-as.numeric(as.character(ModsExSem$SemanticCoherence))

dev.off()

# Comparing exclusivity and semantic coherence using ggplot
options(repr.plot.width=7, repr.plot.height=6, repr.plot.res=100)

plotexcoer<-ggplot(ModsExSem, aes(SemanticCoherence, Exclusivity, color = Model))+geom_point(size = 2, alpha = 0.7) + 
  geom_text(aes(label=K), nudge_y=.04)+
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence")
plotexcoer


#Highest word probabilities for each topic

td_beta <- tidytext::tidy(stm_sdr_44)

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

betaplotT1<-ggplot(betaT1[betaT1$beta>0.005,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity")+coord_flip()+labs(x ="Terms", y = expression(beta),
                                                                                  title = "Word probabilities for Topic 1")#plot word probabilities higher than 0.003 for topic 1

options(repr.plot.width=9, repr.plot.height=10, repr.plot.res=100)
betaplotT1





