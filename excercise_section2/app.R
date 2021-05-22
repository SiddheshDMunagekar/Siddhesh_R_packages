library(tidyverse)
library(RMySQL)
library(dplyr)


####################################################################################
#           UI Module
####################################################################################
sqlUI <- function(id){
  
  
  ns <- NS(id)
  
  tagList(
    
    
    textInput(ns("musician_name"),label="Enter Musician Name",placeholder = "eg. Billie Joe Armstrong"),
    
    uiOutput(ns("name")),
    
    textInput(ns("band_name"),label="Enter Band Name",placeholder = "eg. Green Day"),
    
    
    numericInput(ns("bandid"), label = "Enter Band id", value =2 , min=1),
    
    dateInput(ns("date"),label = "Select the event date",format = "yyyy-mm-dd"),
    actionButton(ns("insert"),label="Insert record"),
    
    
    
    actionButton(ns("submit"),label = "View Combined data"),
    actionButton(ns("view_band"),label = "View Band Table"),
    
    
    dataTableOutput(ns("table")),
    
    
    dataTableOutput(ns("band_table"))
  )

}

##################################################################
##                    server Function
##################################################################
mysqlServer <- function(id,query,record ) {

    
  
  mydb=dbConnect(RMySQL::MySQL(),user='siddhesh',password='Beyond@7seas',
                 dbname='music', port=3306)  
  
  
  
  if (query=='select'){
    
    rs = dbGetQuery(mydb, "Select a.Music_id,a.Musician_name,b.Band_name, a.event_date
                            from musician a
                            inner join band b
                            on a.Band_id=b.Band_id;")
    
    
    ##Close SQL Connection
    all_cons <-dbListConnections(MySQL())
    for(con in all_cons) 
      dbDisconnect(con)
    
    
    return(data.frame(rs)) 
    
  }
  

  if(query=='insert')
  {
    #query <- paste("Insert into musician (Musician_name, Band_id,event_date) values('Arjit Singh',2,'2017-09-01');")
    #record <- list("Siddhesh",5,"2021-05-20","Green day")
    
    insert_query <- paste0("Insert into musician (Musician_name, Band_id,event_date) values(","'",as.character(record[[1]]),"'",",",as.integer(record[[2]]), ",","'",record[[3]],"'",");")
    
    
    ####Executing the Insert Query in   music table
    rsInsert <-dbSendQuery(mydb,insert_query)
    
    dbClearResult(rsInsert)
    
    
    
    band_query <- paste0("Select count(*) from band where band_name=","'",as.character(record[[4]]),"'" ,";")
                            
                            
    
    record_cnt <-  dbGetQuery(mydb,band_query)

    if (as.integer(record_cnt) !=0){
    
    showNotification(paste("Band name" ,as.character(record[[4]]),"already present in band table."), type = "warning")
    }
    else{
      insert_band <- paste0("Insert into band (Band_name) values(","'",as.character(record[[4]]),"'",");")
      ####Executing the Insert Query
      bandInsert <-dbSendQuery(mydb,insert_band)
      
      dbClearResult(bandInsert)
      
    }
    
   
    
    
    
    
    ##Close SQL Connection
    all_cons <-dbListConnections(MySQL())
    for(con in all_cons){
      
      dbDisconnect(con) }
    
    
  }
  
  
  
  
  
  
  
  
  
  
  
  
  if (query=='view_band'){
    
    rs = dbGetQuery(mydb, "Select *
                            from band ;")
    
    
    ##Close SQL Connection
    all_cons <-dbListConnections(MySQL())
    for(con in all_cons) 
      dbDisconnect(con)
    
    
    return(data.frame(rs)) 
    
  }
  
  
  
  
  
}
####################################################################################
#           Server Module
####################################################################################


sqlServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session,id) {
  

      ##############Insert into music table################
      
        
  
  # eventreactive expression
  text_reactive <- eventReactive( input$insert, {
    record <-  c(input$musician_name,
                 as.numeric(input$bandid),
                 as.character(input$date),
                 as.character(input$band_name))
    
    validate(need(input$musician_name, message = paste("Musician name is missing")))
    
    validate(need(input$bandid, message = paste("Band id is missing")))
    
    validate(need(input$band_name, message = paste("Band Name is missing")))
    
    mysqlServer(id,query='insert',record=as.list(record))
    
    
  })
    
  
  
  output$name <- renderUI({
    
    text_reactive()
    
  })        
  
  
  
  
  ##############Select combined  table################
  
  view_music <- eventReactive(input$submit,{
    
    view_table <- mysqlServer(query = 'select') 
  })
  
  
  output$table <-  renderDataTable({
    view_music()
    
    
  })
  ###################select Band table ############
  
  
  view_band <- eventReactive(input$view_band,{
    
    view_band_table <- mysqlServer(query = 'view_band') 
    
    
    
  })
  
 
  output$band_table <-  renderDataTable({
    view_band()
  
  
  }
    
    
  )
  
    
  
  
  
  
  
  
  }

  )
}





#####

#UI
##### 

ui <- fluidPage(
  navbarPage(
    
    
    tabPanel("tab 1",
             
             sqlUI("id"),  
             
    )
    
    
  )
)

######
#server
######

server <- function(input,output,session){
  sqlServer("id")
  
}









################################


shinyApp(ui=ui,server=server)


################################














