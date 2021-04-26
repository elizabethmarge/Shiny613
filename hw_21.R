library(tidyverse)
library(shiny)
ui <- fluidPage(
  h2("BoxPlot and Histogram", style = "color:red"),
  h4("by Elizabeth Marge", style ="color:blue"),
  numericInput(inputId = "n", label = "Sample size", value = 25),
  plotOutput(outputId = "boxplot"),
  plotOutput(outputId = "histogramplot")
)

server <- function(input, output) {
  output$boxplot <- renderPlot({
    boxplot(rnorm(input$n))
  })
  
  
  output$histogramplot <- renderPlot({
    x <- rnorm(input$n)
    bins <- seq(min(x), max(x), length.out = rnorm(input$n) + 1 )
    hist(x, col = "grey", border = "white",
         xlab = "rnorm(input$n)",
         main = "Histogram of rnorm(input$n)")
  })
}   

shinyApp(ui = ui, server = server)