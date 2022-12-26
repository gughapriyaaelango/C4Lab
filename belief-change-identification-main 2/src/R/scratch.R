library(googledrive)
library(lubridate)
library(tidyverse)
library(RcppRoll)
library(future)
library(promises)

BASE_FOLDER = "@Projects/@Belief landscape framework/BLF-Individual/Data/ForThePaper/"
SRCS = list("CNN","Reason","NPR","CrooksAndLiars")#,"OANN")

# Functions --------------------------------------------------------
drive_auth(path = "./src/R/twitter-analysis//gdrive-credentials.json")

read_folder <- function(file) {
  path = str_c(BASE_FOLDER,file)
  folder_id = drive_get(path,shared_drive="C4 Lab")
  #return(drive_ls(folder_id))
  dl<- drive_ls(folder_id) %>%
    filter(endsWith(name,".csv")) %>% 
    mutate(stub = str_extract(name,"[^.]+"),
           type = ifelse(str_detect(name,"topic_defs"),'topic_defs',
                         ifelse(str_detect(name,"topic"),"topics",
                                ifelse(str_detect(name,"annotation"),"annotations","main_id")))
    ) %>% 
    mutate(source = file) %>%
    rowwise() %>% 
    mutate(size = as.numeric(ifelse(type=="main_id",drive_resource$size,0)))
  
  return(dl)
}

read_g_drive<-function() {
  
  #return(read_csv("sample_catalog.csv"))
  
  result <- bind_rows(lapply(SRCS,read_folder)) %>% select(-drive_resource,-name) %>% mutate(id = as.character(id)) %>% 
    pivot_wider(id_cols = c(stub,source), names_from = type, values_from=id, unused_fn = sum)
  labels = c('topic_defs','topics','annotations','main_id') 
  i<-intersect(labels,names(result))
  need<-setdiff(i,labels)
  for (l in need) {
    result[l] = NA
  }
  return(result)
}


prepTopicReps<-function(data,topicdata) {
  d<-data %>% select(topic,word,weight) %>% filter(topic > -1) %>% group_by(topic) %>%
    arrange(-weight,by_group=T) %>%
    summarise(definition = str_c(word,collapse=", ")) %>%
    mutate(topic = str_c("topic_",topic))
  
  t<- topicdata %>% select(-id) %>% summarise(across(.fns=sum)) %>% 
    pivot_longer(names_to="topic",values_to="weight",everything())
  
  d %>% inner_join(t) %>% arrange(-weight)
  
}


prepSourceData<-function(choices, s) {
  choices %>% filter(source==s) %>% 
    mutate(topics = !is.na(topics)) %>% 
    select(stub,topics,size)
}

prepTopicData<-function(data) {
  names(data)[2:length(data)]=str_c("topic_",names(data[2:length(data)]))
  return(data)
}

load_data<-function(data,selected_source,index) {
  row <- (data %>% filter(source == selected_source))[index,]
  result = list()
  result$fileStub = row$stub
  print(paste("Loading ",row$main_id))
  result$twitterData = drive_read_string(as_id(row$main_id)) %>% read_csv()
  
  
  if (is.na(row$annotations)) {
    result$annotationData = tibble(id=double(),note=character())
    id = createAnnotationFile(source,row$stub,result$annotationData)
    data[index,'annotations']<-id
    result$updatedChooserData<-data
  } else {
    result$annotationData = drive_read_string(as_id(row$annotations)) %>% read_csv(col_types = cols(id = col_double())) 
  }
  
  if (is.na(row$topics)) {
    result$topics = NULL
  } else {
    result$topicData = drive_read_string(as_id(row$topics))  %>% read_csv() %>% prepTopicData()
  }
  
  if (is.na(row$topic_defs)) {
    result$topicReps = NULL
  } else {
    result$topicReps = drive_read_string(as_id(row$topic_defs))  %>% read_csv()
  }
  print(paste("Done Loading ",row$main_id))
  return(result)
}


# load_data<-function(data,source,index) {
#   row <- (data %>% filter(source == source))[index,]
#   result = list()
#   result$twitterData = drive_read_string(as_id(row$main_id)) %>% read_csv()
#   
#   if (is.na(row$annotations)) {
#     result$annotationData = NULL
#   } else {
#     result$annotationData = drive_read_string(as_id(row$annotations))
#   }
#   
#   if (is.na(row$topics)) {
#     result$topics = NULL
#   } else {
#     result$topicData = drive_read_string(as_id(row$topics))  %>% read_csv() %>% prepTopicData()
#   }
#   
#   if (is.na(row$topic_defs)) {
#     result$topicReps = NULL
#   } else {
#     result$topicReps = drive_read_string(as_id(row$topic_defs))  %>% read_csv()
#   }
#   return(result)
# }
  
 
get_data<-function(id) {
  drive_read_string(as_id(id)) %>% read_csv()
}


prep_activity_data <- function(data,topicData) {
  if (!is.null(topicData)) {
    d<-data %>% left_join(topicData) 
  }
  
  d %>% select(created_at,starts_with("topic")) %>% mutate(count = 1) %>% 
    mutate(day = as.Date(floor_date(created_at,unit="day"))) %>% 
    group_by(day) %>% summarise(across(c(count,starts_with("topic")),~ sum(.x,na.rm=T))) -> d
  
  seq_dates <-
    tibble(day = seq.Date(
      from = min(d$day),
      to = max(d$day),
      by = "day"
    ))
  d <-
    d %>% right_join(seq_dates) %>% replace(is.na(.),0)
  return(d)
}

roll_activity_data <- function(data, win_size = 5, by = 1) {
  d <- data %>% arrange(day)
  subset <- d %>% select(count,starts_with("topic"))
  rolled <- roll_sum(as.matrix(subset),n=win_size,by=by)
  win_ends <- roll_max(1:nrow(d), n = win_size, by = by)
  return(bind_cols(tibble(day = d$day[win_ends]),as_tibble(rolled)))
}

plot_topic_data <- function(d,selectedTopics=NULL,selectedDate = NULL, normalize = FALSE) {
  
  long_data <- d %>% select(day,all_of(selectedTopics)) %>%
    pivot_longer(names_to="topic",values_to="weight",starts_with("topic_"))
  
  if (normalize) {
    long_data %>% group_by(day) %>% 
      mutate(weight = ifelse(weight ==0, 0, weight / sum(weight)))->long_data
  }
  if (is.null(selectedTopics)) {
    num_topics = 0
  } else {
    num_topics = length(selectedTopics)
  }
  
  # Going to use a trick here to make sure I get distant colors next to one another
  cols <- hue_pal()(num_topics)
  half <- 1:ceiling(length(cols) / 2)
  cols <-
    lighten(muted(as.vector(rbind(cols[half], cols[-half]))), .5)
  g <-
    ggplot(long_data) + geom_area(aes(x = day, y = weight, fill = topic)) + scale_fill_manual(values = cols)+theme_minimal()
  if (num_topics > 14) {
    g = g+ theme(legend.position = "none")
  } 
  
  # else {
  #   g = g + guides(fill = guide_legend(ncol = 2))
  # }
  # 
  if (!is.null(selectedDate)) {
    g = g+geom_vline(xintercept = selectedDate,color="black")
  }
  return(g)
}


createAnnotationFile<-function(directory,name,data) {
  path = str_c(BASE_FOLDER,directory)
  folder_id = drive_get(path,shared_drive="C4 Lab")
  write_csv(data,"tmpfile.csv")
  return(as.character(drive_upload("tmpfile.csv",as_id(folder_id),name,overwrite = T)$id))
}

updateFile<-function(id,data) {
  write_csv(data,"tmpfile.csv")
  future(drive_update(as_id(id),'tmpfile.csv')) %...>% print("Done update")
  return(0)
}





