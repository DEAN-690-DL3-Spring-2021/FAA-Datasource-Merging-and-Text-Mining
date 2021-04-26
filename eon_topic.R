library(stm)
library(tm)
library(stminsights)
library(lubridate)
library(wordcloud)

eon_data = read.csv('eon_stm.csv',colClasses = c("NULL","NULL",NA,NA,"NULL",NA,NA))
str(eon_data)

sum(eon_data$Aircraft_Type=='')

eon_data$Aircraft_Type[eon_data$Aircraft_Type=='UNKN'] <- 'UNKNOWN'

eon_data$Aircraft_Type[eon_data$Aircraft_Type=='TYPE UNKNOWN'] <- 'UNKNOWN'

eon_data$Aircraft_Type[eon_data$Aircraft_Type=='UNKNOWN TYPE'] <- 'UNKNOWN'

eon_data$Aircraft_Type[eon_data$Aircraft_Type==''] <- 'UNKNOWN'

identifier_eon <- read.csv('eon_stm.csv',colClasses = c("NULL",NA,"NULL","NULL","NULL","NULL","NULL"))
str(identifier_eon)

eon_data$Month_Of_Occurrence <- factor(month(as.Date(eon_data$Date_Of_Occurrence),label=TRUE),ordered=FALSE) 

eon_data$Event.Type <- as.factor(eon_data$Event.Type)

processed_eon <- textProcessor(eon_data$Remarks,stem = FALSE,onlycharacter = TRUE,wordLengths= c(4,Inf),
                               customstopwords= c('reported','aircraft','declared','pilot','aborted','issue',
                                                  'notified','acft','departure','departed',
                                                  'diverted','flight','feet','reporting','final','bound','unknown',
                                                  'requested','airport','authorization','arrival'),metadata=eon_data)

out<- prepDocuments(processed_eon$documents, processed_eon$vocab, processed_eon$meta,lower.thresh = 0)

docs_eon <- out$documents
vocab_eon <- out$vocab
meta_eon <- out$meta

stm_eon_25 <- stm(out$documents, out$vocab, K=25, prevalence=~Aircraft_Type+Event.Type+Month_Of_Occurrence, 
                       max.em.its=10, data=out$meta, init.type="Spectral",gamma.prior='L1', seed=1111)

stm_eon_20 <- stm(out$documents, out$vocab, K=20, prevalence=~Aircraft_Type+Event.Type+Month_Of_Occurrence, 
                  max.em.its=10, data=out$meta, init.type="Spectral",gamma.prior='L1', seed=1111)


stm_eon_30 <- stm(out$documents, out$vocab, K=30, prevalence=~Aircraft_Type+Event.Type+Month_Of_Occurrence, 
                  max.em.its=10, data=out$meta, init.type="Spectral",gamma.prior='L1', seed=1111)

plot(stm_eon_25,n=5)

plot(stm_eon_25, type="hist")

topicQuality(model=stm_eon_25, documents=out$documents)

cloud_eon <- cloud(stm_eon_25)

topics_eon <- labelTopics(stm_eon_25)

for (i in 1:dim(topics_eon$score)[1]){
  
  a=c()
  
  for (j in 1:dim(topics_eon$score)[2]){
    
    a = append(a,topics_eon$score[i,j])
    
  }
  
  a = paste(as.character(a), collapse=",")
  assign(paste("Topic", i, sep = ""), a)
  
}

tab_eon <- make.dt(model=stm_eon_25, meta = eon_data)

str(tab_eon)

tab_eon[, Dominant_Topic_eon := colnames(.SD)[max.col(.SD, ties.method="first")], .SDcols = c("Topic1", "Topic2","Topic3","Topic4","Topic5","Topic6","Topic7","Topic8","Topic9","Topic10","Topic11","Topic12","Topic13","Topic14","Topic15",'Topic16','Topic17','Topic18','Topic19','Topic20','Topic21','Topic22','Topic23','Topic24','Topic25')]

tab_eon <- as.data.frame(tab_eon)

topics = list(Topic1,Topic2,Topic3,Topic4,Topic5,Topic6,Topic7,Topic8,Topic9,Topic10,Topic11,Topic12,Topic13,Topic14,Topic15,Topic16,Topic17,Topic18,Topic19,Topic20,Topic21,Topic22,Topic23,Topic24,Topic25)

for (i in 1:dim(topics_eon$score)[1]){
  
  tab_eon$Dominant_Topic_eon[which(tab_eon$Dominant_Topic_eon == paste('Topic',i,sep=''))] = topics[[i]]
  
}

str(tab_eon)

final_eon_topic <- subset(tab_eon,select=c('Dominant_Topic_eon'))

# colnames(final_eon_topic)[colnames(final_eon_topic) == 'Event.Type'] <- 'Event Type'

final_eon_topic$Unique_Identifier <- identifier_eon$Unique_Identifier

str(final_eon_topic)

write.csv(final_eon_topic,'final_eon_topic.csv',row.names = FALSE)

# tab_aid$Dominant_Topic

# kResult <- searchK(out$documents, out$vocab, K=c(5:20), prevalence=~Phase_Of_Flight+s(Month_Of_Occurrence),
#                    data=out$meta,gamma.prior='L1', max.em.its=1, seed=1111)
# 
# plot(kResult)

# save.image('AID_Topic.RData')

# meta_eon$Event.Type <- as.factor(meta_eon$Event.Type)

prep <- estimateEffect(1:25 ~Event.Type+Month_Of_Occurrence, stm_eon_25, meta=out$meta, 
                       uncertainty="Global")

save.image('EON_Topic.RData')

run_stminsights()


# Comparing exclusivity and semantic coherence
suppressWarnings(library(ggplot2))
install.packages("htmlwidgets")
suppressWarnings(library(htmlwidgets))

M25ExSem<-as.data.frame(cbind(c(1:25),exclusivity(stm_eon_25), semanticCoherence(model=stm_eon_25, docs_eon), "Mod25"))
M20ExSem<-as.data.frame(cbind(c(1:20),exclusivity(stm_eon_20), semanticCoherence(model=stm_eon_20, docs_eon), "Mod20"))
M30ExSem<-as.data.frame(cbind(c(1:30),exclusivity(stm_eon_30), semanticCoherence(model=stm_eon_30, docs_eon), "Mod30"))

ModsExSem<-rbind(M20ExSem, M25ExSem, M30ExSem)
colnames(ModsExSem)<-c("K","Exclusivity", "SemanticCoherence", "Model")

ModsExSem$Exclusivity<-as.numeric(as.character(ModsExSem$Exclusivity))
ModsExSem$SemanticCoherence<-as.numeric(as.character(ModsExSem$SemanticCoherence))

options(repr.plot.width=7, repr.plot.height=6, repr.plot.res=100)

plotexcoer<-ggplot(ModsExSem, aes(SemanticCoherence, Exclusivity, color = Model))+geom_point(size = 2, alpha = 0.7) + 
  geom_text(aes(label=K), nudge_y=.04)+
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence")
plotexcoer

#Theta values per document
install.packages("reshape2")
suppressWarnings(library(tidytext))# the package sometimes is not loaded correctly. If this happens, you might have to re-start the kernel 
td_theta <- tidytext::tidy(stm_eon_25, matrix = "theta")

selectiontdthteta<-td_theta[td_theta$document%in%c(1:25),]#select the first 30 documents. be careful to select a sensible interval, as attempting to load a very huge corpus might crash the kernel

thetaplot1<-ggplot(selectiontdthteta, aes(y=gamma, x=as.factor(topic), fill = as.factor(topic))) +
  geom_bar(stat="identity",alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ document, ncol = 5) +
  labs(title = "Theta values per document",
       y = expression(theta), x = "Topic")

options(repr.plot.width=8, repr.plot.height=7, repr.plot.res=100)
thetaplot1

#Highest word probabilities for each topic
suppressWarnings(library(dplyr))
suppressWarnings(library(drlib))#drlib is available on github and needs devtools to be installed

td_beta <- tidytext::tidy(stm_eon_25)

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
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")

#Comparing exclusivity and semantic coherence for model with 15 topics
betaT1<-td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%filter(topic=="Topic 1")#beta values for topic 1

betaplotT1<-ggplot(betaT1[betaT1$beta>0.003,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity")+coord_flip()+labs(x ="Terms", y = expression(beta),
                                                                                  title = "Word probabilities for Topic 1")#plot word probabilities higher than 0.003 for topic 1

options(repr.plot.width=9, repr.plot.height=10, repr.plot.res=100)
betaplotT1

options(repr.plot.width=7, repr.plot.height=6, repr.plot.res=100)
ggplot(ModsExSem[ModsExSem$Model=="Mod25",], aes(SemanticCoherence, Exclusivity, color = Model))+geom_point(size = 2, alpha = 0.7) + 
  geom_text(aes(label=K), nudge_x=.05, nudge_y=.05)+
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence for model with 25 topics")










