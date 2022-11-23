library(shiny)
library(shinyjs)
# library(tidyverse)
library(dplyr)
library(readxl)
library(networkD3)
library(circlize)
library(shinydashboard)
library(googleVis)
library(heatmaply)
library(shinyHeatmaply)
library(RColorBrewer)
library(pals)
# library(VennDiagram)

dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem( tabName = "sankey","Sankey diagram", icon = icon("dashboard"),
                          numericInput("thres13","Threshold per line (left)",value=1,min=1,step=1),
                          numericInput("thres11","Threshold per line (right)",value=1,min=1,step=1),
                          uiOutput("var3"),
                          numericInput("thres23","Threshold per variable",value=1,min=1,step=1),
                          uiOutput("var2"),
                          # numericInput("thres12_1","thres12_1",value=1,min=1,step=1),
                          numericInput("thres22_1","Threshold per variable (left)",value=1,min=1,step=1),
                          # numericInput("thres12_2","thres12_2",value=1,min=1,step=1),
                          numericInput("thres22_2","Threshold per variable (right)",value=1,min=1,step=1),
                          uiOutput("var1"),
                          numericInput("thres21","Threshold per variable",value=1,min=1,step=1)
                ),
                menuItem( tabName = "chord","Chord diagram", icon = icon("th"),
                          selectInput("chord_plot","Type of Chart",choices = c("chord","heatmap")),
                          # uiOutput("var1_2"),
                          numericInput("thres1_2","Threshold per line",value=1,min=1,step=1),
                          uiOutput("var1_2"),
                          numericInput("thres1_2_2","Threshold per variable",value=1,min=1,step=1),
                          uiOutput("var2_2"),
                          numericInput("thres2_2_2","Threshold per variable",value=1,min=1,step=1),
                          numericInput("fontSize2","Font Size",value=8,min=1,step=1)
                ),
                
                menuItem( tabName = "barplot","Barplot", icon = icon("th"),
                          selectInput("bar_plot","Type of Chart",choices = c("Bar","Pie",'Column')),
                          uiOutput("var1_3"),
                          numericInput("thres1_3","Threshold",value=1,min=1,step=1),
                          numericInput("fontSize3","Font Size",value=10,min=1,step=1)
                ),
                menuItem( tabName = "time","Time series", icon = icon("th"),
                          numericInput("thres1_5","Threshold",value=5,min=1,step=1),
                          uiOutput("var1_5"),
                          numericInput("fontSize5","Font Size",value=8,min=1,step=1)
                ),
                menuItem( tabName = "venn","Venn Diagram", icon = icon("th"),      
                          uiOutput("var1_4")
                )
    )
    
    # # Custom CSS to hide the default logout panel
    # ,tags$head(tags$style(HTML('.shiny-server-account { display: none; }')))
    
    # The dynamically-generated user panel
  ),
  dashboardBody(
    
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    # sankeyNetworkOutput('plt1'),
    # plotOutput('plt2', width = "100%"),
    # htmlOutput('plt3'),
    fluidRow(
      tabBox(
        tabPanel("sankey",
                 sankeyNetworkOutput('plt1')
        ),
        tabPanel( "chord",
                  uiOutput("plt2_holder")
                  # plotlyOutput("plt2_2"),
                  # plotOutput('plt2')
        ),
        tabPanel( "barplot",
                  htmlOutput('plt3'),
                  hr(),
                  dataTableOutput('dt3')
        ),
        tabPanel( "time",
                  htmlOutput("plt5")
        ),
        tabPanel( "venn",
                  # plotOutput('plt4'),
                  imageOutput("plt4_2")
        )
      )
    )
  )
  
)