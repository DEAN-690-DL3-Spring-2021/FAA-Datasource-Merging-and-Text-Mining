library(stm)
library(tm)
library(stminsights)
library(lubridate)
library(wordcloud)
install.packages("stminsights")

aid_data = read.csv('aid_stm.csv',colClasses = c("NULL",NA,NA,NA,NA))
str(aid_data)

identifier_aid <- read.csv('aid_stm.csv',colClasses = c(NA,"NULL","NULL","NULL","NULL"))
str(identifier_aid)

aid_data$Month_Of_Occurrence <- factor(month(as.Date(aid_data$Date_Of_Occurrence),label=TRUE),ordered=FALSE) 

aid_data$Phase_Of_Flight <- as.factor(aid_data$Phase_Of_Flight)
aid_data$Aircraft_Make <- as.factor(aid_data$Aircraft_Make)

processed_aid <- textProcessor(aid_data$Remarks, stem= FALSE,onlycharacter = TRUE, wordLengths= c(4,Inf),
                               customstopwords=c('aircraft','airplane','pilot','flight','airport','found', 'airman','time','regulations','prevailed','approximately','without','faa','federal','stated','sustained','inspected','came','rest','plan','reported','name'),
                               metadata=aid_data)

out <- prepDocuments(processed_aid$documents, processed_aid$vocab, processed_aid$meta,lower.thresh = 0)

docs_aid <- out$documents
vocab_aid <- out$vocab
meta_aid <- out$meta

stm_aid_15 <- stm(out$documents, out$vocab, K=15, prevalence=~Aircraft_Make+Phase_Of_Flight+Month_Of_Occurrence,
                  max.em.its=10, data=out$meta, init.type="Spectral",gamma.prior='L1',seed=1111)

stm_aid_20 <- stm(out$documents, out$vocab, K=20, prevalence=~Aircraft_Make+Phase_Of_Flight+Month_Of_Occurrence,
                  max.em.its=10, data=out$meta, init.type="Spectral",gamma.prior='L1',seed=1111)

stm_aid_25 <- stm(out$documents, out$vocab, K=25, prevalence=~Aircraft_Make+Phase_Of_Flight+Month_Of_Occurrence,
                  max.em.its=10, data=out$meta, init.type="Spectral",gamma.prior='L1',seed=1111)

kResult <- searchK(out$documents, out$vocab, K=c(10:25), prevalence=~Aircraft_Make+Phase_Of_Flight+Month_Of_Occurrence,
                   data=out$meta,gamma.prior='L1', max.em.its=10, seed=1111)

plot(kResult)

plot(stm_aid_25,n=7)

plot(stm_aid_25, type="hist")

cloud_aid <- cloud(stm_aid_25)

topics_aid <- labelTopics(stm_aid_25,n=7)

topics_aid

for (i in 1:dim(topics_aid$frex)[1]){
  
  a=c()
  
  for (j in 1:dim(topics_aid$frex)[2]){
    
    a = append(a,topics_aid$frex[i,j])
    
  }
  
  a = paste(as.character(a), collapse=",")
  assign(paste("Topic", i, sep = ""), a)
  
}

tab_aid <- make.dt(model=stm_aid_25, meta = aid_data)

tab_aid[, Dominant_Topic_aid := colnames(.SD)[max.col(.SD, ties.method="first")], .SDcols = c("Topic1", "Topic2","Topic3","Topic4","Topic5","Topic6","Topic7","Topic8","Topic9","Topic10","Topic11","Topic12","Topic13","Topic14","Topic15",
                                                                                              "Topic16","Topic17","Topic18","Topic19","Topic20","Topic21","Topic22","Topic23","Topic24","Topic25")]

tab_aid <- as.data.frame(tab_aid)

topics = list(Topic1,Topic2,Topic3,Topic4,Topic5,Topic6,Topic7,Topic8,Topic9,Topic10,Topic11,Topic12,Topic13,Topic14,Topic15,
              Topic16,Topic17,Topic18,Topic19,Topic20,Topic21,Topic22,Topic23,Topic24,Topic25)

for (i in 1:25){
  
  tab_aid$Dominant_Topic_aid[which(tab_aid$Dominant_Topic_aid == paste('Topic',i,sep=''))] = topics[[i]]
  
}

str(tab_aid)

final_aid_topic <- subset(tab_aid,select=c('Dominant_Topic_aid'))

final_aid_topic$Unique_Identifier <- identifier_aid$Unique_Identifier

str(final_aid_topic)

write.csv(final_aid_topic,'final_aid_topic.csv',row.names = FALSE)

# tab_aid$Dominant_Topic



# save.image('AID_Topic.RData')

# meta_aid$Phase_Of_Flight <- as.factor(meta_aid$Phase_Of_Flight)

prep <- estimateEffect(1:25 ~Aircraft_Make+Phase_Of_Flight+Month_Of_Occurrence, stm_aid_25, meta=out$meta, 
                       uncertainty="Global")

save.image('AID_Topic.RData')

#run_stminsights()

# Comparing exclusivity and semantic coherence
suppressWarnings(library(ggplot2))
#install.packages("htmlwidgets")
suppressWarnings(library(htmlwidgets))

M25ExSem<-as.data.frame(cbind(c(1:25),exclusivity(stm_aid_25), semanticCoherence(model=stm_aid_25, docs_aid), "Mod25"))
M15ExSem<-as.data.frame(cbind(c(1:15),exclusivity(stm_aid_15), semanticCoherence(model=stm_aid_15, docs_aid), "Mod15"))
M20ExSem<-as.data.frame(cbind(c(1:20),exclusivity(stm_aid_20), semanticCoherence(model=stm_aid_20, docs_aid), "Mod20"))

ModsExSem<-rbind(M25ExSem, M15ExSem, M20ExSem)
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
td_theta <- tidytext::tidy(stm_aid_25, matrix = "theta")

selectiontdthteta<-td_theta[td_theta$document%in%c(1:25),]#select the first 25 documents. be careful to select a sensible interval, as attempting to load a very huge corpus might crash the kernel

thetaplot1<-ggplot(selectiontdthteta, aes(y=gamma, x=as.factor(topic), fill = as.factor(topic))) +
  geom_bar(stat="identity",alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ document, ncol = 3) +
  labs(title = "Theta values per document",
       y = expression(theta), x = "Topic")

options(repr.plot.width=8, repr.plot.height=7, repr.plot.res=100)
thetaplot1

#Highest word probabilities for each topic
suppressWarnings(library(dplyr))
#suppressWarnings(library(drlib))#drlib is available on github and needs devtools to be installed

td_beta <- tidytext::tidy(stm_aid_25)

options(repr.plot.width=7, repr.plot.height=8, repr.plot.res=100)

td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
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

#Comparing exclusivity and semantic coherence for model with 25 topics
betaT1<-td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%filter(topic=="Topic 1")#beta values for topic 1

betaplotT1<-ggplot(betaT1[betaT1$beta>0.003,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity")+coord_flip()+labs(x ="Terms", y = expression(beta),
                                                                                  title = "Word probabilities for Topic 1")#plot word probabilities higher than 0.003 for topic 1

options(repr.plot.width=9, repr.plot.height=10, repr.plot.res=100)
betaplotT1

options(repr.plot.width=7, repr.plot.height=6, repr.plot.res=100)
ggplot(ModsExSem[ModsExSem$Model=="Mod15",], aes(SemanticCoherence, Exclusivity, color = Model))+geom_point(size = 2, alpha = 0.7) + 
  geom_text(aes(label=K), nudge_x=.05, nudge_y=.05)+
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence for model with 25 topics")

# Estimate Effect 
out$meta
out$meta$Month_Of_Occurrence<-as.factor(out$meta$Month_Of_Occurrence)
prep<-estimateEffect(1:25~ Month_Of_Occurrence, stm_aid_25, metadata=out$meta, uncertainty="Global")#nsim is defaulted to 25, but on a small model a higher number lead to more consistent results
summary(prep, topics=c(1:3), nsim=1000)# summary of regression on topic 1-3

options(repr.plot.width=7, repr.plot.height=7, repr.plot.res=100)
plot.estimateEffect(prep, model=stm_aid_25, covariate="Month_Of_Occurrence", topics=3, 
                    method="pointestimate", 
                    xlim=c(-.5,1))



library(ggthemes)

top_terms <- td_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

gamma_terms %>%
  top_n(20, gamma) %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 3,
            family = "IBMPlexSans") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.09),
                     labels = percent_format()) +
  theme_tufte(base_family = "IBMPlexSans", ticks = FALSE) +
  theme(plot.title = element_text(size = 16,
                                  family="IBMPlexSans-Bold"),
        plot.subtitle = element_text(size = 13)) +
  labs(x = NULL, y = expression(gamma),
       title = "Top 20 topics by prevalence in the Hacker News corpus",
       subtitle = "With the top words that contribute to each topic")

