library(babynames)
library(tidyverse)
library(shiny)

ui <- fluidPage(textInput(inputId = "name",
                          label = "Name:",
                          value = "",
                          placeholder = "Siddhesh"),
                
                selectInput(inputId = "sex",
                            label=  "Gender:",
                            choices=list(Female="F", Male="M")),
                
                sliderInput(inputId = "year",
                            label = "Year Range:",
                            min= min(babynames$year),
                            max=max(babynames$year),
                            value=c(min(babynames$year),max(babynames$year)),sep=""),
                submitButton(text="Create my plot"),
                plotOutput(outputId = "nameplot")
               
                )

server <- function(input, output){
  output$nameplot<-renderPlot({babynames %>% filter(sex==input$sex, name==input$name) %>% 
       ggplot(aes(x= year, y=n))+ 
      geom_line()+
      scale_x_continuous(limits = input$year)+
      theme_bw()})
  
}

shinyApp(ui= ui, server= server)


