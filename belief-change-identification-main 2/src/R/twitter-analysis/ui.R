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



#Shiny App ------------------------------------------------
js_code <- HTML("shinyjs.seque = function(par) {
                var dt = $('#table2 table').DataTable(); 
                var start = par[0][0]
                var end = par[0][1]
                var indices = []
                for (var i = start;i<end+1;i++) {
                  indices.push(i);
                }
                //dt.page(20).draw('page');
                //dt.ajax.reload(null,false);
                dt.rows().deselect();
                
                dt.rows(indices).select();
                var info = dt.page.info()
                var desired_page = Math.floor(start / info.length);
                if (desired_page != info.page) {
                  dt.page(desired_page).draw('page');
                }
                
                //var rows = dt.rows( '.selected' ).indexes().length;
                //console.log('There are '+rows+' selected');
           }")

shinyUI(fluidPage(
  useShinyjs(),
  extendShinyjs(text = js_code, functions = "seque"),
  navbarPage(
    theme = shinythemes::shinytheme("flatly"),
    title = "Visualizing Belief Change in Twitter Users",
    
   
    
    tabPanel(
      textOutput("filestub"),
      sidebarLayout(
        sidebarPanel(
          
            id = 'dataset',
            tabsetPanel(id="chooserTab",
              tabPanel("CNN", withSpinner(DT::dataTableOutput("CNN")),type=4),
              tabPanel("NPR", withSpinner(DT::dataTableOutput("NPR")),type=4),
              tabPanel("Reason", withSpinner(DT::dataTableOutput("Reason")),type=4),
              tabPanel("CrooksAndLiars", withSpinner(DT::dataTableOutput("CrooksAndLiars")),type=4)
          ),
          actionButton("loadData", "Load Data",
                       style="width:100%;"),
          # fileInput(
          #   "file1",
          #   "Choose CSV file",
          #   multiple = FALSE,
          #   accept = ("text/csv")
          # ),
          sliderInput(
            "bin_by",
            "Bin by:",
            min = 1,
            max = 25,
            value = 7
          ),
          sliderInput(
            "slide_by",
            "Slide by:",
            min = 1,
            max = 25,
            value = 7
          )
         
        ),
        mainPanel(tabsetPanel(
          id="vizTab",
          tabPanel("Activity",
                   fluidRow(
                     titlePanel(h1("Twitter Activity")),
                     column(
                       width = 12,
                       class = "well",
                       plotOutput(
                         "plot1",
                         height = 600,
                         click = "Activity_click"
                       )
                     )
                   )),
          tabPanel("Topics",
                   fluidRow(
                     titlePanel(h1("Topic Dynamics")),
                     column(
                       width = 12,
                       class = "well",
                       checkboxInput("normalizeCheckbox", label = "Normalized", value = FALSE),
                       withSpinner(plotOutput(
                         "plot2",
                         height = 600,
                         click = "Topics_click"
                       ),type=4),
                       DTOutput("topicDefs")
                     )
                   )),
          
          tabPanel("Weighted Jaccard",
                   fluidRow(
                     titlePanel(h1("Weighted Jaccard Similarity")),
                     column(
                       width = 12,
                       class = "well",
                       plotOutput(
                         "plot3",
                         height = 600,
                         dblclick = "plot3_dblclick",
                         click = "plot3_click",
                         brush = brushOpts(id = "plot3_brush",
                                           resetOnNew = TRUE)
                       )
                     ),
                     column(width = 12, class = "well", DTOutput("table3"))
                   )),
          tabPanel("Cosine Similarity",
                   fluidRow(
                     titlePanel(h1("Cosine Similarity")),
                     column(
                       width = 12,
                       class = "well",
                       plotOutput(
                         "plot4",
                         height = 600,
                         dblclick = "plot4_dblclick",
                         click = "plot4_click",
                         brush = brushOpts(id = "plot4_brush",
                                           resetOnNew = TRUE)
                       )
                     ),
                     column(width = 12, class = "well", DTOutput("table4"))
                   )),
          tabPanel("Entropy",
                   fluidRow(
                     titlePanel(h1("Entropy")),
                     column(
                       width = 12,
                       class = "well",
                       plotOutput(
                         "plot5",
                         height = 600,
                         dblclick = "plot5_dblclick",
                         click = "plot5_click",
                         brush = brushOpts(id = "plot5_brush",
                                           resetOnNew = TRUE)
                       )
                     ),
                     column(width = 12, class = "well", DTOutput("table5"))
                   ))
          
        ))),
      textInput("text", "Search"),
      DT::dataTableOutput("table2"),
      ui <- fluidPage(
        useMarker(),
        tags$head(
          tags$style(
            ".red{background-color:#FFB8C3;}"
          )
        )
     
        
      )
   
      
    )
  )
))



