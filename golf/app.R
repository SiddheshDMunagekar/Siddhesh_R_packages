library(shiny)
library(shinythemes)
library(data.table)
library(RCurl)
library(randomForest)
library(readr)
library(visNetwork)
library(rsconnect)
library('httr')
library(RCurl)


######Should we play golf?#################

#google drive url
temp <- tempfile(fileext = ".zip")



url <- 'https://drive.google.com/file/d/1b8-p7DTUFi6LE_TZoR9Sbz2Ye6exIN5i/view?usp=sharing'

path <-  paste0('https://drive.google.com/uc?export=download&id=',strsplit(url,"/")[[1]][6])

#strsplit(url, "/")[[1]][6]

#Load dataset
golf_data <- read_csv(path)

golf_data <- data.frame(golf_data)
# Build model

golf_data$temperature <- as.integer(golf_data$temperature)
golf_data$humidity <-  as.integer(golf_data$humidity)
str(golf_data)

#Converting categorical columns to factors
golf_data$play <- factor(golf_data$play,level=c("yes","no"))
golf_data$outlook <- factor(golf_data$outlook, levels = c("sunny","rainy","overcast"))


#Building the model
model <- randomForest(play ~.,data=golf_data, ntree=500, mtry=4, importance=TRUE)


ui <- fluidPage(theme = shinytheme("united"),
                
                # Page header
                headerPanel('Should we play Golf?'),
                
                # Input values
                sidebarPanel(
                  
                  HTML("<h3>Input parameters</h3>"),
                  
                  selectInput("outlook", label = "Outlook:", 
                              choices = list("Sunny" = "sunny", "Overcast" = "overcast", "Rainy" = "rainy"), 
                              selected = "Sunny"),
                  sliderInput("temperature", "Temperature: F scale",
                              min = 64, max = 86,
                              value = 70),
                  sliderInput("humidity", "Humidity:",
                              min = 65, max = 96,
                              value = 90),
                  selectInput("windy", label = "Windy:", 
                              choices = list("Yes" = "TRUE", "No" = "FALSE"), 
                              selected = "TRUE"),
                  
                  actionButton("submitbutton", "Decide", class = "btn btn-primary")
                ),
                
                mainPanel(
                  tags$label(h3('Decision Output')), # Status/Output Text Box
                  verbatimTextOutput('contents'),
                  tableOutput('tabledata') # Prediction results table
                  
                )
)

####################################
# Server                           #
####################################

server <- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    # outlook,temperature,humidity,windy,play
    df <- data.frame(
      Name = c("outlook",
               "temperature",
               "humidity",
               "windy"),
      Value = as.character(c(input$outlook,
                             input$temperature,
                             input$humidity,
                             input$windy)),
      stringsAsFactors = FALSE)
    
    play <- "play"
    df <- rbind(df, play)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    test$outlook <- factor(test$outlook, levels = c("overcast", "rainy", "sunny"))
    
    
    Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
    print(Output)
    
  })
  
  
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

####################################
# Create the shiny app             #
####################################

shinyApp(ui = ui, server = server)