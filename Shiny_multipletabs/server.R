library("kableExtra")
library(tidyverse)
library(DT)
library(ggplot2)
library(lubridate)

countries_and_continent <- read_csv("countries.csv") %>% 
  rename(country=name)

wdi_data <-  read_csv("wdi_data.csv")

server <-  function(input, output,session){
  
  output$curve_plot <-  renderPlot({
    curve(x^input$exponent, from =-5 ,to =5)
    
  })
  
  output$star_narrow <- function(){
    starwars %>% 
      select(name,species,homeworld, height) %>% 
      arrange(desc(height)) %>% 
      kable() %>% 
      kable_styling(bootstrap_options = c("striped","hover"))
    
  }

  #DT table
  output$star_wide <- renderDT({
    starwars %>% select(name, height, homeworld) %>% 
      arrange(desc(height)) %>% 
      datatable(rownames = input$show_rownames,extensions = "Responsive")
  })
  ##############
  

  
  updateSelectInput(session,
                    "selected_continent",
                    choices = unique(countries_and_continent$continent))
  

  
observeEvent(c(input$selected_continent),
             {
               countries_in_continent <-  countries_and_continent %>% 
                                          filter(continent==input$selected_continent) %>% 
                                          select(country)
               
               updateSelectInput(session,
                                 "selected_country",
                                 choices = countries_in_continent)
               
             } )


output$internet_users_by_country <- renderPlot({
                wdi_data %>% 
                filter(country ==input$selected_country, indicator=="IT.NET.USER.ZS") %>% 
                filter(!is.na(value)) %>% 
                ggplot(aes(x= year, y=value))+ geom_smooth()
    
                
  
  
})

###Insert Data and Analyse###
  
output$time_tracker_table_rhandson <- renderRHandsontable({
  tibble("employee_name"="",
         "project_name"="",
         "hours_worked"="",
         "date"=today()) %>% 
  rhandsontable(colHeaders = c("Employee Name",
                               "Project Name",
                               "Hours Worked",
                               "Date"),
                rowHeaders = NULL)  
    
})

inserted_time_tracking <-   eventReactive(c(input$analyse_data),{
  
  if(input$analyse_data == 0){
    return()
  }
  
  time_tracker <- input$time_tracker_table_rhandson %>% 
    hot_to_r() %>% 
    as_tibble() %>% 
    mutate(hours_worked=as.numeric(hours_worked))
  
  
})

output$timetracker_summary <- renderPlot({
  inserted_time_tracking <- inserted_time_tracking()
  
  if(input$analyse_data == 0){
    return()
  }
  
  inserted_time_tracking %>%
       group_by(project_name) %>%
       summarise(total_hours = sum(hours_worked)) %>%
       ggplot(aes(x = project_name,
                  y = total_hours)) +
       geom_col(fill='light blue',color='black') +
       labs(x = "",
            y = "Total Hours Worked") +
       theme_minimal() +
       coord_flip()
})


}


