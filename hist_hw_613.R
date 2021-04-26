
library(shiny)
library(tidyverse)

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
    hist(rnorm(input$n))
  })
}

shinyApp(ui = ui, server = server)
