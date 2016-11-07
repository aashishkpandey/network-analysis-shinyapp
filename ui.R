####################################################
#      Network App    #
####################################################

library("shiny")
library("igraph")
#library("foreign")

shinyUI(pageWithSidebar(
  # Header:
  headerPanel("Network App"),
  # Input in sidepanel:
  sidebarPanel(

    h5(p("Data Input")),
    fileInput("file", "Upload Adjacency Matrix (csv file with header))"),
    fileInput("file1", "Upload Demographics data (csv file with header))"),
    selectInput("mode","Mode of Graph",c("directed", "undirected","max", "min", "upper",
                                         "lower", "plus"),"undirected"),
    selectInput("comm","Find Communities",c("Yes", "No"),"No"),
    htmlOutput("yvarselect"),
    sliderInput("cex", "Data point labels font size", min = 0.1,  max = 3, value = 1,round = FALSE),
    sliderInput("cex2", "Vertex Size", min = 0.1,  max = 20, value = 5,round = FALSE),
    br()
  ),
  # Main:
  mainPanel( 
    
    tabsetPanel(type = "tabs",
                #
                tabPanel("Network Plot",plotOutput("graph1", height = 800, width = 840)),
                tabPanel("Communities Plot",
                plotOutput("graph2", height = 800, width = 840),
                uiOutput("graph3")), #, height = 800, width = 840
                tabPanel("Network Centralities",br(),
                         downloadButton('downloadData1', 'Download Centralities file (Works only in browser)'), br(),br(),
                         dataTableOutput("centdata"))
                )
            ) 
        ) 
    )
