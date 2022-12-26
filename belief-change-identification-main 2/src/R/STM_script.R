process_user <- function(filename) {
  data = read.csv(filename)
  root_name = str_split(filename,".csv")[[1]][1]
  
  # 1. Pre-processing text content 
  processed <- textProcessor(data$text,metadata=data)
  out <- prepDocuments(processed$documents,
                       processed$vocab,
                       processed$meta)
  
  
  docs<- out$documents
  vocab <- out$vocab
  meta <- out$meta
  
  # 2. Lee and Mimno(2014) method to find K
  leenmimno <- stm(documents=out$documents,
                   vocab=out$vocab,
                   K=0,
                   prevalence =~created_at,
                   max.em.its = 75,
                   data=out$meta,
                   init.type= "Spectral", 
                   verbose = FALSE,
                   gamma.prior='L1')
  
  # 3. Topic Definitions
  td_beta<-tidy(second_stm, matrix="beta") %>% arrange(topic,beta)%>% group_by(topic)%>% top_n(n=10)
  
  # 3.2. Topic Definitions - list of words per topic
  lables <- labelTopics(leenmimno)
  
  topic_defs<- data.frame(
    topic=lables$topicnums,
    definition=lables$frex
  )
  
  topic_defs<- topic_defs %>% unite(col="definitions", c(2:ncol(topic_defs)), sep=",")
  
  # 4. STM Topics
  topicprop <- make.dt(leenmimno, meta)
  topic <- topicprop %>% select(as.factor("id") ,starts_with("Topic"))
  
  # 5. Write Files
  topic_fname = paste0(root_name,"_stm_topics.csv")
  topic_defs_fname = paste0(root_name,"_stm_topic_defs.csv")
  topic_defs_list_fname = paste0(root_name,"_stm_topic_defs_list.csv")
  
  write.csv(topic,topic_fname)
  write.csv(td_beta,topic_defs_fname)
  write.csv(topic_defs,topic_defs_list_fname)
}
