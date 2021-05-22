library(shiny)
library("kableExtra")
library(DT)
library("shinyjs")
#install.packages("shinyjs")
library("rhandsontable")
library("shinycustomloader")
#install.packages("shinycustomloader")

ui <-  fluidPage(
  navbarPage(
    "My shiny",
    tabPanel("Tab 1",
             sliderInput("exponent",label="Choose exponent", min =1, max=5, value =2),
             
             plotOutput("curve_plot")),
    tabPanel("Star Narrow",
             fluidPage(
               tableOutput("star_narrow")
             )
             ),
    tabPanel(
      "Star Wide",
      fluidPage(
        checkboxInput("show_rownames",label = "Show rownames"),
        DTOutput("star_wide")
      )
    ),
    
   
    tabPanel("Internet Users across Countries & Continent",
            fluidPage(
              fluidRow(
                column(selectInput("selected_continent","Select Continent",choices = NULL), width = 6),
                
                column(selectInput("selected_country","Select Country", choices = NULL), width = 6)
              )
              
            ),
            withLoader(plotOutput("internet_users_by_country"))
            ),
    
    tabPanel("Insert data and analyse",
             fluidPage(
               
               rHandsontableOutput("time_tracker_table_rhandson"),
               br(),
               actionButton("analyse_data",
                            "Analyse Data",
                            width = "100%"),
               plotOutput("timetracker_summary")
               
               
             )),
    
    collapsible  = TRUE
    
  )
)
