library(tidyverse)
library(R.utils)
gunzip("47339454_CNN.csv.gz", remove=FALSE)

CNN <- read.csv("47339454_CNN.csv")

install.packages("stm")
install.packages("igraph")
install.packages("stmCorrViz")
library(stm)


# 1)Ingestion
processed <- textProcessor(CNN$text,metadata=CNN)
out <- prepDocuments(processed$documents, 
                     processed$vocab, 
                     processed$meta)

docs<- out$documents
vocab <- out$vocab
meta <- out$meta


# 2)Preparation
# 2-1)first_stm; assign random 'n'topics to K for a testing
first_stm <- stm(documents=out$documents,
                 vocab=out$vocab,
                 K=10,
                 prevalence =~created_at,
                 max.em.its = 75,
                 data=meta,
                 init.type= "Spectral",
                 verbose = FALSE,
                 gamma.prior='L1')
                 
plot(first_stm, type="summary", xlim=c(0,.4))


# 2-2)lee and mimno method; K=0 while init.type="Spectral"

install.packages("geometry")
install.packages("Rtsne")
install.packages("rsvd")
library(geometry)

leenmimno <- stm(documents=out$documents,
                 vocab=out$vocab,
                 K=0,
                 prevalence =~created_at,
                 max.em.its = 75,
                 data=meta,
                 init.type= "Spectral",
                 verbose = FALSE,
                 gamma.prior='L1')
plot(leenmimno)


# 2-3)searchK function; evaluate which topic model number is the best out of options

findingK <- searchK(out$documents, 
                    out$vocab, 
                    K=c(10,20,30,40,50,60,70), 
                    prevalence= ~created_at, 
                    data=meta, 
                    verbose=FALSE,
                    gamma.prior='L1')
plot(findingK)

# 3)Testing Exclusivity and Semantic Coherence

#20 topic models
select <- selectModel(out$documents, out$vocab, K=20,
                      prevalence = ~created_at,
                      max.em.its =75,
                      data= meta,
                      runs=20,
                      seed=8458159,
                      gamma.prior='L1')

plotModels(select, pch=c(9,10,11,12), legend.position = "bottomright")

#30 topic models
select3 <- selectModel(out$documents, out$vocab, K=30,
                      prevalence = ~created_at,
                      max.em.its =75,
                      data= meta,
                      runs=30,
                      seed=8458159,
                      gamma.prior='L1')

plotModels(select3, pch=c(1,2,3,4), legend.position = "bottomright")

#40 topic models
select4 <- selectModel(out$documents, out$vocab, K=40,
                       prevalence = ~created_at,
                       max.em.its =75,
                       data= meta,
                       runs=40,
                       seed=8458159,
                       gamma.prior='L1')

plotModels(select4, pch=c(5,6,7,8), legend.position = "bottomright")

#50 topic models
select5 <- selectModel(out$documents, out$vocab, K=50,
                       prevalence = ~created_at,
                       max.em.its =75,
                       data= meta,
                       runs=50,
                       seed=8458159,
                       gamma.prior='L1')
plotModels(select5, pch=c(5,6,7,8), legend.position = "bottomright")

#check stability of particular topics across models
selectedmodel <- select4$runout[[5]]
plot(selectedmodel)

# 4) second_stm with the most appropriate topic number n
second_stm <- stm(documents=out$documents,
                 vocab=out$vocab,
                 K=40,
                 prevalence =~created_at,
                 max.em.its = 75,
                 data=meta,
                 init.type= "Spectral",
                 verbose = FALSE,
                 gamma.prior='L1')
plot(second_stm, type="summary")


library(broomExtra)
install.packages("broomExtra")
install.packages("broom.mixed")
library(broom.mixed)
library(tidyverse)
library(stm)
library(tidytext)
install.packages("tidytext")
library(dplyr)


# 5) Generate Files : topic numbers and definitions

labels<- labelTopics(second_stm)
topwords2 <- data.frame(
  topic=labels$topicnums,
  definition=labels$frex)
  
topwords3 <-topwords2 %>%
 mutate(definition=paste(definition.1, definition.2,definition.3,definition.4, 
       definition.5, definition.6)) %>%
 summarise(topic,definition)
  
write.csv(topwords3,"C:\\Users\\leeta\\OneDrive - Syracuse University\\label.csv")

# 6) Generate Files : weight score per topic

topicprop <- make.dt(second_stm, meta)
topic2<-topicprop %>% 
  summarise(id,Topic1,Topic2,Topic3,Topic4,Topic5,Topic6,Topic7,Topic8,Topic9,Topic10,
            Topic11,Topic12,Topic13,Topic14,Topic15,Topic16,Topic17,Topic18,Topic19,
            Topic20,Topic21,Topic22,Topic23,Topic24,Topic25,Topic26,Topic27,Topic28,
            Topic29,Topic30)

topic2$id <- as.factor(topic2$id)
topic2

write.csv(topic2,"C:\\Users\\leeta\\OneDrive - Syracuse University\\topic.csv")
