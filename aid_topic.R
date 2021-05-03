#setting work directory
setwd("~/AID")
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
aid_data = read.csv('aid_stm.csv',colClasses = c("NULL",NA,NA,NA,NA))
str(aid_data)

identifier_aid <- read.csv('aid_stm.csv',colClasses = c(NA,"NULL","NULL","NULL","NULL"))
str(identifier_aid)

# changing into factors
aid_data$Month_Of_Occurrence <- factor(month(as.Date(aid_data$Date_Of_Occurrence),label=TRUE),ordered=FALSE) 
aid_data$Phase_Of_Flight <- as.factor(aid_data$Phase_Of_Flight)
aid_data$Aircraft_Make <- as.factor(aid_data$Aircraft_Make)

## textProcessor prepares the corpus, representing the documents as lists containting 
#word indicese and associated word counts, the vocab character vector associated with 
#the word indices and a metadata matrix containing the covariates. As we can remove some custom words, 
#here we opt to remove "aircraft" , "inspector" and so on from the corpus 

processed_aid <- textProcessor(aid_data$Remarks, stem= FALSE,onlycharacter = TRUE, wordLengths= c(4,Inf),
                               customstopwords=c('aircraft','airplane','pilot','flight','airport','found', 'airman','time','regulations','prevailed','approximately','without','faa','federal','stated','sustained','inspected','came','rest','plan','reported','name',
                                                 "landed", "never", "land", "plane", "final", "around", "took",  "post", "area", "data", "feet","take", "took", "taxi","will", "report", "declared", "made","instructor","indicated","interview","conducted",
                                                 "impacted","command","inspector"),
                               metadata=aid_data)

#PrepDocuments is a helpful function to perform some manipulations like removing words based on thresholds 
#without compromising the indexes 

out <- prepDocuments(processed_aid$documents, processed_aid$vocab, processed_aid$meta,lower.thresh = 0)

docs_aid <- out$documents
vocab_aid <- out$vocab
meta_aid <- out$meta

# For finding the optimal Number of Topics
kResult <- searchK(out$documents, out$vocab, K=c(10:25), prevalence=~Aircraft_Make+Phase_Of_Flight+Month_Of_Occurrence,
                   data=out$meta,gamma.prior='L1', max.em.its=10, seed=1111)

plot(kResult)

#
stm_aid_15 <- stm(out$documents, out$vocab, K=15, prevalence=~Aircraft_Make+Phase_Of_Flight+Month_Of_Occurrence,
                       max.em.its=10, data=out$meta, init.type="Spectral",gamma.prior='L1',seed=1111)

stm_aid_20 <- stm(out$documents, out$vocab, K=20, prevalence=~Aircraft_Make+Phase_Of_Flight+Month_Of_Occurrence,
                  max.em.its=10, data=out$meta, init.type="Spectral",gamma.prior='L1',seed=1111)

stm_aid_25 <- stm(out$documents, out$vocab, K=25, prevalence=~Aircraft_Make+Phase_Of_Flight+Month_Of_Occurrence,
                  max.em.its=10, data=out$meta, init.type="Spectral",gamma.prior='L1',seed=1111)


# plot can be used to visualize the topic distribution 
plot(stm_aid_25,n=7)

#topic visualization

topic_model <- stm_aid_25 
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
  top_n(15, gamma) %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE,fill = "goldenrod2") +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 3,
            family = "IBMPlexSans") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.15),
                     labels = percent_format())+
  #scale_fill_brewer(fill = "goldenrod2")+
theme_tufte(base_family = "IBMPlexSans", ticks = FALSE) +
  theme(plot.title = element_text(size = 10,
                                  family="IBMPlexSans-Bold"),
        plot.subtitle = element_text(size = 8)) +
  labs(x = "Proportion", y = "Topics",
       title = "Top 25 topics by prevalence in the AID Dataset",
       subtitle = "With the top words that contribute to each topic")



# The plot.STM function with "hist" as argument in order to visualize the estimates of document-topic proportions

plot.STM(stm_aid_25, type="hist")

cloud_aid <- cloud(stm_aid_25)

#labelTopics gives us more detailed insights on the popular words in each topic. 
topics_aid <- labelTopics(stm_aid_25,n=7)

topics_aid

# creating new column with Dominant Topic Keywords based on Frex metric

for (i in 1:dim(topics_aid$frex)[1]){
  
  a=c()
  
  for (j in 1:dim(topics_aid$frex)[2]){
    
    a = append(a,topics_aid$frex[i,j])
 
  }
  
  a = paste(as.character(a), collapse=",")
  assign(paste("Topic", i, sep = ""), a)
  
}

#Making a data table of topic proportions
tab_aid <- make.dt(model=stm_aid_25, meta = aid_data)

#Assigning each document with its dominant topic
tab_aid[, Dominant_Topic_aid := colnames(.SD)[max.col(.SD, ties.method="first")], .SDcols = c("Topic1", "Topic2","Topic3","Topic4","Topic5","Topic6","Topic7","Topic8","Topic9","Topic10","Topic11","Topic12","Topic13","Topic14","Topic15",
                                                                                              "Topic16","Topic17","Topic18","Topic19","Topic20","Topic21","Topic22","Topic23","Topic24","Topic25")]
#converting into a data frame
tab_aid <- as.data.frame(tab_aid)

#making a list of all the topics
topics = list(Topic1,Topic2,Topic3,Topic4,Topic5,Topic6,Topic7,Topic8,Topic9,Topic10,Topic11,Topic12,Topic13,Topic14,Topic15,
              Topic16,Topic17,Topic18,Topic19,Topic20,Topic21,Topic22,Topic23,Topic24,Topic25)

#keywords for each topic
for (i in 1:25){
  
  tab_aid$Dominant_Topic_aid[which(tab_aid$Dominant_Topic_aid == paste('Topic',i,sep=''))] = topics[[i]]
  
}

str(tab_aid)

#Making a subset of the original data frame to contain only dominant topic keywords.
final_aid_topic <- subset(tab_aid,select=c('Dominant_Topic_aid'))

#Adding a new column unique identifier to the final_aid_topic file from original aid_data
final_aid_topic$Unique_Identifier <- identifier_aid$Unique_Identifier
  
str(final_aid_topic)

#importing into CSV
write.csv(final_aid_topic,'final_aid_topic.csv',row.names = FALSE)

# Estimate effect
# The function to extract the relationship and associated uncertainty on all the topics of the model is 
# estimateEffect, which basically estimates a regression with documents as units, the outcome is the proportion
# of each document about a topic in an STM model and the covariates are document-meta data

prep <- estimateEffect(1:25 ~Aircraft_Make+Phase_Of_Flight+Month_Of_Occurrence, stm_aid_25, meta=out$meta, 
                       uncertainty="Global")

save.image('AID_Topic.RData')

# STM insights Dashboard
run_stminsights()

# Comparing exclusivity and semantic coherence
suppressWarnings(library(ggplot2))
#install.packages("htmlwidgets")
suppressWarnings(library(htmlwidgets))

#creating dataframes for each model with their respective semantic coherence and exclusivity scores
M25ExSem<-as.data.frame(cbind(c(1:25),exclusivity(stm_aid_25), semanticCoherence(model=stm_aid_25, docs_aid), "Mod25"))
M15ExSem<-as.data.frame(cbind(c(1:15),exclusivity(stm_aid_15), semanticCoherence(model=stm_aid_15, docs_aid), "Mod15"))
M20ExSem<-as.data.frame(cbind(c(1:20),exclusivity(stm_aid_20), semanticCoherence(model=stm_aid_20, docs_aid), "Mod20"))

# combining the rows of all three dataframes
ModsExSem<-rbind(M25ExSem, M15ExSem, M20ExSem)

#renaming column names for the above dataframe
colnames(ModsExSem)<-c("K","Exclusivity", "SemanticCoherence", "Model")

#converting SemanticCoherence and Exclusivity of the dataframe to numeric datatype
ModsExSem$Exclusivity<-as.numeric(as.character(ModsExSem$Exclusivity))
ModsExSem$SemanticCoherence<-as.numeric(as.character(ModsExSem$SemanticCoherence))

options(repr.plot.width=7, repr.plot.height=6, repr.plot.res=100)

# Comparing exclusivity and semantic coherence using ggplot
plotexcoer<-ggplot(ModsExSem, aes(SemanticCoherence, Exclusivity, color = Model))+geom_point(size = 2, alpha = 0.7) + 
  geom_text(aes(label=K), nudge_y=.04)+
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence")
plotexcoer


#Highest word probabilities for each topic

#suppressWarnings(library(drlib))#drlib is available on github and needs devtools to be installed

td_beta <- tidytext::tidy(stm_aid_25)

options(repr.plot.width=7, repr.plot.height=8, repr.plot.res=100)

td_beta %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
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
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity")+coord_flip()+labs(x ="Terms", y = "Probability",
                                                                                  title = "Word probabilities for Topic 1")

options(repr.plot.width=9, repr.plot.height=10, repr.plot.res=100)
betaplotT1






