library(htmltools)
library(shiny)
library(tidyverse)


ui <- fluidPage(
  titlePanel("mtcars data"),
  verbatimTextOutput("code"), 
  checkboxGroupInput("mtcars", "For which mtcars varaible is mean>median?", 
                     choices = mtcars) 
)

server <- function(input, output) {
  output$code <- renderPrint({
    summary(mtcars)
  })
}

shinyApp(ui = ui, server = server)



