library(tidyverse)
library(RMySQL)
library(dplyr)
library(cats)


#' A MySQL Operation
#'
#' This function allows you to select records from the table and to insert records to the music table.
#' @param select, Select records of musician from the database .
#' @param insert,record ,Insert records into musican table where insert is string and record is a list of values.
#' 
#' @export
#' @examples
#' mysql_operations('select') for selecting records
#' mysql_operations('insert',record) for inserting records into music table



mysql_operations <- function(query,record){
  
  mydb=dbConnect(MySQL(),user='root',password='Beyond@7seas',
                 dbname='music',host='127.0.0.1')  


  
if (query=='select'){
  
rs = dbGetQuery(mydb, "select * from musician;")
#print("You are in select query")


##Close SQL Connection
all_cons <-dbListConnections(MySQL())
for(con in all_cons) 
  dbDisconnect(con)


return(data.frame(rs)) 

}

  
  
if(query=='insert')
  {
    #query <- paste("Insert into musician (Musician_name, Band_id,event_date) values('Arjit Singh',2,'2017-09-01');")
    #record <- list("Siddhesh",5,"2021-05-20")
  
  insert_query <- paste0("Insert into musician (Musician_name, Band_id,event_date) values(","'",as.character(record[[1]]),"'",",",as.integer(record[[2]]), ",","'",record[[3]],"'",");")
  
    
    ####Executing the Insert Query
    rsInsert <-dbSendQuery(mydb,insert_query)
    
     dbClearResult(rsInsert)
    
    
    ##Close SQL Connection
    all_cons <-dbListConnections(MySQL())
    for(con in all_cons){
      
      dbDisconnect(con) }
  
    
    }
 
  
  

}


                 
                 
                 