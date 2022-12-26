library(shiny)
library(shinyjs)
library(viridis)
library(hrbrthemes)
library(lubridate)
library(DT)
library(plotly)
library(tidyverse)
library(colorspace)
library(scales)
library(RcppRoll)
library(gghighlight)
library(googledrive)
library(shinycssloaders)
library(shinyalert)
library(reactlog)
library(future)
library(marker)

BASE_FOLDER = "@Projects/@Belief landscape framework/BLF-Individual/Data/ForThePaper/"
SRCS = list("CNN","Reason","NPR","CrooksAndLiars")#,"OANN")
options(scipen=999)
reactlog::reactlog_enable()

# Functions --------------------------------------------------------
#drive_auth(path = "accessgdrive-346418-7ae76fc21bfd.json")
drive_auth(path = "./gdrive-credentials.json")

# Helpert function -----------------------------------------------
mynearpoints <- function (df, coordinfo, xvar = NULL, yvar = NULL, panelvar1 = NULL, 
          panelvar2 = NULL, threshold = 5, maxpoints = NULL, addDist = FALSE, 
          allRows = FALSE) 
{
  if (is.null(coordinfo)) {
    if (addDist) 
      df$dist_ <- NA_real_
    if (allRows) 
      df$selected_ <- FALSE
    else df <- df[0, , drop = FALSE]
    return(df)
  }
  if (is.null(coordinfo$x)) {
    stop("nearPoints requires a click/hover/double-click object with x and y values.")
  }
 
  xvar <- xvar %||% coordinfo$mapping$x
  yvar <- yvar %||% coordinfo$mapping$y
  panelvar1 <- panelvar1 %||% coordinfo$mapping$panelvar1
  panelvar2 <- panelvar2 %||% coordinfo$mapping$panelvar2
  if (is.null(xvar)) 
    stop("nearPoints: not able to automatically infer `xvar` from coordinfo")
  if (is.null(yvar)) 
    stop("nearPoints: not able to automatically infer `yvar` from coordinfo")
  if (!(xvar %in% names(df))) 
    stop("nearPoints: `xvar` ('", xvar, "')  not in names of input")
 
  coordinfo <- shiny:::fortifyDiscreteLimits(coordinfo)
  x <- shiny:::asNumber(df[[xvar]], coordinfo$domain$discrete_limits$x)
  #y <- shiny:::asNumber(df[[yvar]], coordinfo$domain$discrete_limits$y)
  point_img <- coordinfo$coords_img
  data_img <- shiny:::scaleCoords(x, 0, coordinfo)
 
  dists <- abs((data_img$x - point_img$x)/coordinfo$img_css_ratio$x)
  if (addDist) 
    df$dist_ <- dists
  keep_rows <- (dists <= threshold)
  if (!is.null(panelvar1)) 
    keep_rows <- keep_rows & shiny:::panelMatch(coordinfo$panelvar1, 
                                        df[[panelvar1]])
  if (!is.null(panelvar2)) 
    keep_rows <- keep_rows & shiny:::panelMatch(coordinfo$panelvar2, 
                                        df[[panelvar2]])
  keep_idx <- which(keep_rows)
  dists <- dists[keep_idx]
  keep_idx <- keep_idx[order(dists)]
  if (!is.null(maxpoints) && length(keep_idx) > maxpoints) {
    keep_idx <- keep_idx[seq_len(maxpoints)]
  }
  if (allRows) {
    df$selected_ <- FALSE
    df$selected_[keep_idx] <- TRUE
  }
  else {
    df <- df[keep_idx, , drop = FALSE]
  }
  df
}


#Data Prep --------------------------------------------------------



prepTopicReps<-function(data,topicdata) {
  d<-data %>% select(topic,word,weight) %>% filter(topic > -1) %>% group_by(topic) %>%
    arrange(-weight,by_group=T) %>%
    summarise(definition = str_c(word,collapse=", ")) %>%
    mutate(topic = str_c("topic_",topic))
  
  t<- topicdata %>% select(-id) %>% summarise(across(.fns=sum)) %>% 
    pivot_longer(names_to="topic",values_to="weight",everything())
  
  d %>% inner_join(t) %>% arrange(-weight)
  
}


prep_text_data<-function(d,annotationData) {
  return(d %>% arrange(created_at) %>% mutate(day = as.Date(floor_date(created_at, unit = "day"))) %>% select(id,day,text) %>% 
           left_join(annotationData))
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

roll_activity_data2 <- function(data, win_size = 5, by = 1) {
  d <- data %>% arrange(day)
  subset <- d %>% select(count,starts_with("topic"))
  rolled <- as_tibble(apply(subset,2,function(x) roll_sum(x,n=win_size,by=by)))
  win_ends <- roll_max(1:nrow(d), n = win_size, by = by)
  return(bind_cols(tibble(day = d$day[win_ends]),as_tibble(rolled)))
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



# Loading / Saving data

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
  return(result %>% filter(!is.na(main_id)))
}

load_data<-function(data,selected_source,index) {
  row <- (data %>% filter(source == selected_source))[index,]
  result = list()
  result$fileStub = row$stub
  #print(paste("Loading ",row$main_id))
  result$twitterData = drive_read_string(as_id(row$main_id)) %>% read_csv()
  
  
  if (is.na(row$annotations)) {
    result$annotationData = tibble(id=double(),note=character())
    id = createAnnotationFile(selected_source,row$stub,result$annotationData)
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
  #print(paste("Done Loading ",row$main_id))
  return(result)
}



createAnnotationFile<-function(directory,stub,data) {
  path = str_c(BASE_FOLDER,directory)
  folder_id = drive_get(path,shared_drive="C4 Lab")
  write_csv(data,"tmpfile.csv")
  return(as.character(drive_upload("tmpfile.csv",as_id(folder_id),str_c(stub,".annotation.csv"),overwrite = T)$id))
}

updateFile<-function(id,data) {
  write_csv(data,"tmpfile.csv")
  future(drive_update(as_id(id),'tmpfile.csv'))
  return(0)
}



#Plotting  --------------------------------------------------------

plot_activity_data<-function(d,selection) {
  g<-d %>% select(day,count) %>% ggplot() + geom_area(aes(day,count),fill="lightblue")+theme_minimal()
  if (!is.null(selection)) {
    return(g+geom_vline(xintercept = selection,color="black"))
  } else {
    return(g)
  }
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

#UNUSED --------------------------------------------------------

# bin_data <- function(long_data, num_days, normalize = F) {
#   d <-
#     long_data %>% ungroup() %>% mutate(index = floor(as.numeric(day - min(day)) / num_days)) %>% group_by(index, topic) %>% summarise(weight = sum(weight), day = min(day)) %>% ungroup() %>% select(-index)
#   if (normalize) {
#     d %>% group_by(day) %>% mutate(weight = weight / sum(weight)) -> d
#   }
#   return(d)
# }

#Roll Data --------------------------------

# roll_data <-
#   function(long_data,
#            win_size = 5,
#            by = 1,
#            normalize = F) {
#     # To make life easier, I'm going to pivot my long data to wide
#     wd <-
#       pivot_wider(long_data, names_from = topic, values_from = weight) %>% arrange(day)
#     rolled <-
#       as_tibble(apply(wd %>% select(starts_with("topic_")), 2, function(x)
#         roll_mean(x, n = win_size, by = by)))
#     
#     win_ends <- roll_max(1:nrow(wd), n = win_size, by = by)
#     
#     rolled$day = wd$day[win_ends]
#     r <-
#       rolled %>% select(day, everything()) %>% pivot_longer(names_to = "topic",
#                                                             values_to = "weight",
#                                                             starts_with("topic_"))
#     if (normalize) {
#       r %>% group_by(day) %>% mutate(weight = weight / sum(weight)) -> r
#     }
#     return(r)
#     
#   }


############  DATA ANALYSIS FUNCTIONS

# Weighted Jaccards -----------------------------------

weighted_jaccard <- function(x, y) {
  n <- sum(pmin(x, y))
  d <- sum(pmax(x, y))
  ifelse(d == 0, 0, n / d)
}

# Presume our data has already been binned / rolled
calc_topic_churn <- function(long_data) {
  long_data %>% group_by(topic) %>% arrange(day, .by_group = TRUE) %>% mutate(lagged_weights = lag(weight, 1, order_by = day)) -> lagged_data
  #return(lagged_data)
  lagged_data %>% filter(!is.na(lagged_weights)) %>% group_by(day) %>% summarise(jaccard = weighted_jaccard(weight, lagged_weights))
}

# Cosine Similarity -------------------------------------------

cosine_similarity <- function(x, y) {
  if (length(x) != length(y)) {
    stop("x and y must be equal length vectors")
  }
  n = sum(x * y)
  d = sqrt(sum(x ^ 2)) * sqrt(sum(y ^ 2))
  ifelse(d == 0, 0, n / d)
}

# Presume our data has already been binned / rolled
calc_cosine_similarity <- function(long_data) {
  long_data %>% group_by(topic) %>% arrange(day, .by_group = TRUE) %>% mutate(lagged_weights = lag(weight, 1, order_by = day)) -> lagged_data
  #return(lagged_data)
  lagged_data %>% filter(!is.na(lagged_weights)) %>% group_by(day) %>% summarise(similarity = cosine_similarity(weight, lagged_weights))
}

# Entropy ---------------------------------------------------------

entropy <- function(x, base = exp(1)) {
  p = x / sum(x)
  - sum(p * log(p, base))
}

# Presume our data has already been binned / rolled
calc_entropy <- function(long_data) {
  long_data %>% group_by(day) %>% summarise(entropy = entropy(weight))
}

# Skew ---------------------------------------------------------

skew <- function(x) {
  if (length(x) == 0) {
    return(0)
  } else {
    p = x / sum(x)
    1 - exp(-sum(p * log(p))) / length(x)
  }
}

# Presume our data has already been binned / rolled
calc_skew <- function(long_data) {
  long_data %>% group_by(day) %>% summarise(skew = skew(weight))
}









shinyServer(function(input, output, session) {
  options(shiny.maxRequestSize = 150 * 1024 ^ 2)
  
  #full_data[, 'Annotations'] <- NA #add annotation column to data
  v <- reactiveValues(
    'fileStub' = NULL,
    "twitterData"=NULL,
                           "topicData" = NULL, 
                          "topicReps" = NULL,
                          "viewTopics" = NULL,
                           "annotationData" = NULL, 
                           "countData"=NULL,
                           'rollData' = NULL,
                           'selectedDate' = NULL,
                           "textData"=NULL)
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  filechoices <- read_g_drive()
  
  
  
  marker <- marker$new("#table2")
  
  observeEvent(input$text, {
    marker$
      unmark(className = "red")$ # unmark red class
      mark(input$text, className = "red") # add red class
      
  })

    
 
  # EVENTS
  observeEvent(ignoreInit = TRUE,c(input$CNN_cell_clicked,input$CrooksAndLiars_cell_clicked,input$Reason_cell_clicked,input$NPR_cell_clicked), {
    selector<-str_c(input$chooserTab,"_rows_selected")
    print(paste("The selector is ",selector))
    selected_row <- input[[selector]]
    if (length(selected_row) > 0) {
      print(paste("Loading ",input$chooserTab))
      d<-load_data(filechoices,input$chooserTab,selected_row)
      v$fileStub = d$fileStub
      if (!is.null(d$updatedChooserData)) {
        filechoices = d$updatedChooserData
      }

      twitterData<-d$twitterData
      countData <-prep_activity_data(twitterData,d$topicData)
      rolledData<-roll_activity_data2(countData,isolate(input$bin_by),isolate(input$slide_by))
      v$countData = countData
      v$twitterData=twitterData
      
      if (!is.null(d$topicData)) {
        v$topicData=d$topicData
        v$topicReps = prepTopicReps(d$topicReps,d$topicData)
        v$viewTopics = v$topicReps$topic[1:min(10,nrow(v$topicReps))]
        print(v$viewTopics)
      } else {
        v$topicData=NULL
        v$topicReps = NULL
        v$viewTopics = NULL
      }
      
      v$annotationData = d$annotationData
      v$rollData = rolledData
      v$selectedDate = NULL
      v$textData = prep_text_data(twitterData,d$annotationData)
      
    
    }
     
  })

 
  
  observeEvent(ignoreInit = TRUE,c(input$bin_by,input$slide_by),{
    validate(need(v$countData,"Please select some data"))
    binning = input$bin_by
    sliding = input$slide_by
    v$rollData <- roll_activity_data2(v$countData, input$bin_by, input$slide_by)
  })
 
  
  observeEvent(ignoreInit = TRUE, c(input$Activity_click,input$Topics_click), {
    
    clicksource = str_c(input$vizTab,"_click")
    print(paste("Clicksource ",clicksource))
    if (!is.null(input[[clicksource]])) {
      x<-mynearpoints(v$rollData,input[[clicksource]],yvar=NULL,maxpoints = 1)
      v$selectedDate = x$day
      f_point = (v$textData %>% group_by(day) %>% summarise(id = first(id)) %>% 
                   mutate(diff = as.numeric(day - x$day)) %>%
                   filter(diff >= 0) %>%
                   filter(diff == min(diff)))$id
      
      idx <- which(v$textData$id == f_point)
      print(idx)
      js$seque(c(idx,idx))
    }
    #y<-brushedPoints()
    #print(x)
    
  })
  

  
  observeEvent(input[["table2_cell_edit"]], {
    cellinfo <- input[["table2_cell_edit"]]
    print("Edit table")
    print(cellinfo)
    tweet_id = as.double(v$textData[cellinfo$row,'id'])
    ndata = tibble(id=tweet_id,note=as.character(cellinfo$value))
    
    v$annotationData = rows_upsert(v$annotationData,ndata,by="id")
    print(v$fileStub)
    print(head(filechoices))
    
    id = (filechoices %>% filter(stub==v$fileStub))$annotations[1]
    updateFile(id,v$annotationData)
  })
  
  observeEvent(ignoreInit = TRUE, input$table2_cell_clicked, {
    selected_row <- isolate(input$table2_rows_selected)
    v$selectedDate = v$textData$day[selected_row]
  })
  
  observeEvent(ignoreInit = TRUE, input$topicDefs_cell_clicked, {
    if (!is.null(input$topicDefs_cell_clicked$row)) {
      selected_rows <- isolate(input$topicDefs_rows_selected)
      print(selected_rows)
      v$viewTopics = v$topicReps$topic[selected_rows]
    }
    #v$viewTopics = 
  })
  
  
  
  
  ################################
  ###
  ###  RENDERING
  ###
  ################################
  
  
  #Render chooser tables
  
  output$CNN <- DT::renderDataTable({
    data <- prepSourceData(filechoices,"CNN")
    DT::datatable(data,selection = 'single', rownames = F)
  })
  
  
  output$Reason <- DT::renderDataTable({
    data <- prepSourceData(filechoices,"Reason")
    DT::datatable(data,selection = 'single', rownames = F)
  })
  
  output$NPR <- DT::renderDataTable({
    data <- prepSourceData(filechoices,"NPR")
    DT::datatable(data,selection = 'single', rownames = F)
  })
  
  
  output$CrooksAndLiars <- DT::renderDataTable({
    data <- prepSourceData(filechoices,"CrooksAndLiars")
    DT::datatable(data,selection = 'single', rownames = F)
  })
  
  
  output$plot1 <- renderPlot({
    validate(need(v$rollData,message = "Please select a dataset"))
    plot_activity_data(v$rollData,v$selectedDate)
  })
  
  output$plot2 <- renderPlot({
    validate(need(v$topicData,message = "Please select a dataset with topics"))
    plot_topic_data(v$rollData,v$viewTopics,v$selectedDate,input$normalizeCheckbox)
  })
 
  output$filestub <- renderText(v$fileStub)
  

  
  output[["table2"]] <- renderDT({
    validate(need(v$textData,"Please select a dataset"))
    datatable(
      v$textData,
      plugins='input',
      editable = "cell",
      class = 'cell-border strip hover',
      extensions = c('Buttons', 'Scroller',"Select","SearchPanes"),
      selection = "single",
      rownames = F,
      options = list(
        #dom = 'Bfrtip',
        dom = 't',
        #searchHighlight = TRUE,
        #search = list(regex = TRUE, caseInsensitive = TRUE),
        pageLength=300000,
        pagingType = 'input',
        searchPanes = list(show = FALSE),
        deferRender = TRUE,
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print','selectAll', 'selectNone', 'selectRows', 'selectColumns', 'selectCells'),
        scroller = FALSE,
        scrollY = 800,
        scrollX = FALSE,
        columnDefs = list(
          list(visible=FALSE, targets=c(0)),
          list(width = '40%', targets = c(2)),
          list(width = '40%', targets = c(3))
        )
      )
      
    ) %>% formatStyle(0, cursor = 'pointer')
  },server = FALSE)

  
  # output[["chooserTable"]] <- renderDT({
  #   
  #   datatable(
  #     v$chooserData,
  #     editable = "cell",
  #     class = 'cell-border strip hover',
  #     extensions = c('Buttons', 'Scroller',"Select"),
  #     rownames = F,
  #     options = list(
  #       dom = 'Bfrtip',
  #       deferRender = TRUE,
  #       buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
  #       scroller = FALSE,
  #       scrollY = 800,
  #       scrollX = FALSE,
  #       columnDefs = list(
  #         list(visible=FALSE, targets=c(0)),
  #         list(width = '40%', targets = c(2)),
  #         list(width = '40%', targets = c(3))
  #       )
  #     )
  #   ) %>% formatStyle(0, cursor = 'pointer')
  # },server = FALSE)
  
  output$topicDefs <- renderDT({
    validate(need(v$topicReps,"Requires a dataset with topic analysis"))
    selected = which(v$topicReps$topic %in% isolate(v$viewTopics))
    datatable(
      v$topicReps,
      class = 'cell-border strip hover',
      extensions = c('Scroller',"Select"),
      selection = list(mode = 'multiple', selected = selected,target="row"),
      rownames = F,
      options = list(
        dom = 'ltip',
        deferRender = TRUE,
        paging=F,
        scroller = FALSE,
        scrollY = 400,
        scrollX = FALSE,
        columnDefs = list(
          list(width = '10%', targets = c(0)),
          list(width = '10%', targets = c(2))
        )
      )
    ) %>% formatStyle(0, cursor = 'pointer')
  },server = FALSE)
 
 
 
 
 
  
  #--------------------------------------------------------------------------------------------------------------------------
  
  
  
  #SIMILARITY METRICS ---------------------------------------------------------------------------------
  
  
  
  #-----------------------------------------------------------------------------------------------------------
  
  #--------------------------------------------------------------------------- JACCARD
  
  #jaccard
  output$plot3 <- renderPlot({
    rolled_data <- roll_data(v()$topicData, input$win_size, input$bin_by)
    jaccarddata <- calc_topic_churn(rolled_data)
    jaccarddata[, 'Annotations'] <- NA
    ggplot(jaccarddata) + geom_line(aes(day, jaccard)) + theme_minimal() +
      ylim(0, 1)
  })
  
  
  
  observeEvent(input[["table3_cell_edit"]], {
    cellinfo <- input[["table3_cell_edit"]]
    v$dat3 <<-
      editData(v$dat3, input[["table3_cell_edit"]], "table")
  })
  
  observeEvent(input$plot3_dblclick, {
    brush <- input$plot3_brush
    if (!is.null(brush))
    {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
    }
    else
    {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  
  
  
  #---------------------------------------------------------------------------- COSINE
  
  #cosine
  output$plot4 <- renderPlot({
    rolled_data <- roll_data(v()$topicData, input$win_size, input$bin_by)
    cosinedata <- calc_cosine_similarity(rolled_data)
    cosinedata[, 'Annotations'] <- NA
    ggplot(cosinedata) + geom_line(aes(day, similarity)) + theme_minimal() +
      ylim(0, 1) + ggtitle("Cosine similarity")
  })
  
  
  
  observeEvent(input$plot4_dblclick, {
    brush <- input$plot4_brush
    if (!is.null(brush))
    {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
    }
    else
    {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  
  
  #---------------------------------------------------------------------------- ENTROPY
  #entropy
  output$plot5 <- renderPlot({
    rolled_data <- roll_data(v()$topicData, input$win_size, input$bin_by)
    entropydata <- calc_skew(rolled_data)
    entropydata[, 'Annotations'] <- NA
    ggplot(entropydata) + geom_line(aes(day, skew)) + ylim(0, 1) + theme_minimal() +
      ggtitle("Skew")
  })
  
  observeEvent(input$plot5_brush, {
    rolled_data <- roll_data(v()$topicData, input$win_size, input$bin_by)
    entropydata <- calc_skew(rolled_data)
    entropydata[, 'Annotations'] <- NA
    v$dat5 <- brushedPoints(entropydata, input$plot5_brush)
  })
  
  output[["table5"]] <- renderDT({
    validate(need(input$plot5_brush, 'Brush over the plot to select data.'),
    )
    datatable(
      v$dat5,
      editable = "cell",
      class = 'cell-border strip hover',
      extensions = c('Buttons', 'Scroller'),
      options = list(
        dom = 'Bfrtip',
        deferRender = TRUE,
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        scroller = TRUE,
        scrollY = 800,
        scrollX = FALSE
      )
    ) %>% formatStyle(0, cursor = 'pointer')
  })
  
  observeEvent(input[["table5_cell_edit"]], {
    cellinfo <- input[["table5_cell_edit"]]
    v$dat5 <<-
      editData(v$dat5, input[["table5_cell_edit"]], "table")
  })
  
  observeEvent(input$plot5_dblclick, {
    brush <- input$plot5_brush
    if (!is.null(brush))
    {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
    }
    else
    {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  
}
)
