library(stm)
library(tm)
library(stminsights)
library(lubridate)
library(wordcloud)

sdr_data = read.csv('sdr_stm.csv',colClasses = c("NULL","NULL",NA,NA,"NULL",NA,NA))
str(sdr_data)

identifier_sdr <- read.csv('sdr_stm.csv',colClasses = c("NULL",NA,"NULL","NULL","NULL","NULL","NULL"))
str(identifier_sdr)

sdr_data$Month_Of_Occurrence <- factor(month(as.Date(sdr_data$Date_Of_Occurrence),label=TRUE),ordered=FALSE) 

sdr_data$Phase_Of_Flight <- as.factor(sdr_data$Phase_Of_Flight)

processed_sdr <- textProcessor(sdr_data$Remarks,stem = FALSE,onlycharacter = TRUE, wordLengths= c(4,Inf),
                               customstopwords=c('pilot','report','aircraft','classified','major','per','yes','performed','satisfactory','operational','check',
                                                 'accomplish','accomplished','issued','provide','instruction',
                                                 'scheduled','inspection','found'), metadata=sdr_data)

out <- prepDocuments(processed_sdr$documents, processed_sdr$vocab, processed_sdr$meta,lower.thresh = 0)

docs_sdr <- out$documents
vocab_sdr <- out$vocab
meta_sdr <- out$meta

stm_sdr_30 <- stm(out$documents, out$vocab, K=30, prevalence=~Aircraft_Type+Phase_Of_Flight+Month_Of_Occurrence,
                             max.em.its=10, data=out$meta, init.type="Spectral",gamma.prior='L1', seed=1111)

stm_sdr_25 <- stm(out$documents, out$vocab, K=25, prevalence=~Aircraft_Type+Phase_Of_Flight+Month_Of_Occurrence,
                  max.em.its=10, data=out$meta, init.type="Spectral",gamma.prior='L1', seed=1111)

stm_sdr_35 <- stm(out$documents, out$vocab, K=35, prevalence=~Aircraft_Type+Phase_Of_Flight+Month_Of_Occurrence,
                  max.em.its=10, data=out$meta, init.type="Spectral",gamma.prior='L1', seed=1111)


plot(stm_sdr_30,n=5)

plot(stm_sdr_30, type="hist")

cloud_sdr <- cloud(stm_sdr_30)

topics_sdr <- labelTopics(stm_sdr_30)

topics_sdr

topics_sdr$score

topicQuality(model=stm_sdr_30, documents=out$documents)

for (i in 1:dim(topics_sdr$score)[1]){
  
  a=c()
  
  for (j in 1:dim(topics_sdr$score)[2]){
    
    a = append(a,topics_sdr$score[i,j])
    
  }
  
  a = paste(as.character(a), collapse=",")
  assign(paste("Topic", i, sep = ""), a)
  
}

tab_sdr <- make.dt(model=stm_sdr_30, meta = sdr_data)

str(tab_sdr)

tab_sdr[, Dominant_Topic_sdr := colnames(.SD)[max.col(.SD, ties.method="first")], .SDcols = c("Topic1", "Topic2","Topic3","Topic4","Topic5","Topic6","Topic7","Topic8","Topic9","Topic10","Topic11","Topic12","Topic13","Topic14","Topic15",'Topic16','Topic17','Topic18','Topic19','Topic20','Topic21','Topic22','Topic23','Topic24','Topic25','Topic26','Topic27','Topic28','Topic29','Topic30')]

tab_sdr <- as.data.frame(tab_sdr)

topics = list(Topic1,Topic2,Topic3,Topic4,Topic5,Topic6,Topic7,Topic8,Topic9,Topic10,Topic11,Topic12,Topic13,Topic14,Topic15,Topic16,Topic17,Topic18,Topic19,Topic20,Topic21,Topic22,Topic23,Topic24,Topic25,Topic26,Topic27,Topic28,Topic29,Topic30)

for (i in 1:dim(topics_sdr$score)[1]){
  
  tab_sdr$Dominant_Topic_sdr[which(tab_sdr$Dominant_Topic_sdr == paste('Topic',i,sep=''))] = topics[[i]]
  
}

str(tab_sdr)

final_sdr_topic <- subset(tab_sdr,select=c('Dominant_Topic_sdr'))

final_sdr_topic$Unique_Identifier <- identifier_sdr$Unique_Identifier

str(final_sdr_topic)

write.csv(final_sdr_topic,'final_sdr_topic.csv',row.names = FALSE)

# kResult <- searchK(out$documents, out$vocab, K=c(10:30), prevalence=~Aircraft_Type+Phase_of_Flight+s(Date_Of_Occurrence),
#                    data=out$meta,gamma.prior='L1', max.em.its=1, seed=1111)
# 
# plot(kResult)

prep <- estimateEffect(1:30 ~Phase_Of_Flight+Month_Of_Occurrence, stm_sdr_30, meta=out$meta, 
                       uncertainty="Global")

save.image('SDR_Topic.RData')

run_stminsights()


# Comparing exclusivity and semantic coherence
suppressWarnings(library(ggplot2))
install.packages("htmlwidgets")
suppressWarnings(library(htmlwidgets))

M30ExSem<-as.data.frame(cbind(c(1:30),exclusivity(stm_sdr_30), semanticCoherence(model=stm_sdr_30, docs_sdr), "Mod30"))
M25ExSem<-as.data.frame(cbind(c(1:25),exclusivity(stm_sdr_25), semanticCoherence(model=stm_sdr_25, docs_sdr), "Mod25"))
M35ExSem<-as.data.frame(cbind(c(1:35),exclusivity(stm_sdr_35), semanticCoherence(model=stm_sdr_35, docs_sdr), "Mod35"))

ModsExSem<-rbind(M25ExSem, M30ExSem, M35ExSem)
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
td_theta <- tidytext::tidy(stm_sdr_30, matrix = "theta")

selectiontdthteta<-td_theta[td_theta$document%in%c(1:30),]#select the first 30 documents. be careful to select a sensible interval, as attempting to load a very huge corpus might crash the kernel

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

td_beta <- tidytext::tidy(stm_sdr_30)

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
ggplot(ModsExSem[ModsExSem$Model=="Mod30",], aes(SemanticCoherence, Exclusivity, color = Model))+geom_point(size = 2, alpha = 0.7) + 
  geom_text(aes(label=K), nudge_x=.05, nudge_y=.05)+
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence for model with 15 topics")

# Estimate Effect 
out$meta
out$meta$Month_Of_Occurrence<-as.factor(out$meta$Month_Of_Occurrence)
prep<-estimateEffect(1:15~ Month_Of_Occurrence, stm_aid_15, metadata=out$meta, uncertainty="Global")#nsim is defaulted to 25, but on a small model a higher number lead to more consistent results
summary(prep, topics=c(1:3), nsim=1000)# summary of regression on topic 1-3

options(repr.plot.width=7, repr.plot.height=7, repr.plot.res=100)
plot.estimateEffect(prep, model=stm_aid_15, covariate="Month_Of_Occurrence", topics=3, 
                    method="pointestimate", 
                    xlim=c(-.5,1))




