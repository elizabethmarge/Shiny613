## In this project we are using the Shiny Web Application. 
## For each step of our analysis, we will show step by step instructions on how to conduct the Shiny Reactive Apps with our chosen data. 
## We chose the data palmerpenguins. The palmerpenguins (penguins) data set was collected by Dr. Kristen Gorman and the Palmer Station, Antarctica LTER, which is the Long-Term Ecological Research Network. 
## There are two packages within palmerpenguins: penguins and penguins_raw. There is data collected for 344 penguins, and the 3 different species of the penguins in the dataset: Adelie, Chinstrap, and Gentoo. 
## The 3 islands in the dataset are Biscoe, Dream, and Torgersen, in the Palmer Archipelago, Antarctica. 
## Throughout our analysis, we show and create 4 different instances of reactivity. 
## In the first Reactive , we welcome the user for the dataset and learn more about them.  
## In the second Reactive, we introduce the dataset penguins comparing with the packages of Iris and Mtcars. To reiterate, the aim of our project is to introduce and operate palmerpenguins dataset, the iris and Mtcars are only comparison. 
## In the third Reactive, we look at different numeric variables and categorical variables based on different species, sex, islands. 
## Lastly, in the fourth reactive we run a series of plots against the dataset penguins that result in different data visualizations. 
library(tidyverse)
library(shiny)
library("palmerpenguins")
data = "penguins"
data = "penguins_raw"



## In Reactive #1, we welcome the user for the dataset and learn more about them. 

named <- names(penguins)
ui <- fluidPage(
  titlePanel("Welcome to the World of Penguins <(^)"),
  textInput("name", "If you were a penguin, what would your name be?"),
  textOutput("greeting"),
  
  titlePanel("Hi, I am from Antarctica!  "),
  textInput("Hi", "where are you from?"),
  textOutput("islands"),
  
  
  checkboxGroupInput("named", "What are you most interested to learn about regarding penguins?", 
                     choices = named)
  
)

server <- function(input, output, session) {
  output$greeting <- renderText({
    paste("What a cool name for a penguin. Hi ", input$name, "!")
  })
  
  
  output$islands <- renderText({
    paste("I have never left Antarctica, I would love to visit ", input$Hi, " and say hi!")
  })
  
  datasetInput <- reactive({
    mtcars
  })
}

shinyApp(ui = ui, server = server)




## In the second Reactive, we introduce the dataset penguins comparing with Iris and Mtcars.
## To reiterate, the aim of our project is to introduce and operate palmerpenguins dataset, the iris and mtcars are only comparison. 


ui <- fluidPage(
  
  # App title ----
  titlePanel("Penguins dataset with Mtcars and Iris"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      # Input: Text for providing a caption 
      textInput(inputId = "caption",
                label = "Caption:",
                value = "Data Summary"),
      
      # Input: Selector for choosing the choosing dataset penguins  
      selectInput(inputId = "dataset",
                  label = "Choose a dataset:",
                  choices = c("penguins", "iris", "mtcars")),
      
      # Input: Numeric entry for number of observations to view 
      numericInput(inputId = "obs",
                   label = "Number of observations to view:",
                   value = 10)
      
    ),
    
    # Main panel for displaying outputs 
    mainPanel(
      
      # Output: Formatted text for caption 
      h3(textOutput("caption", container = span)),
      
      # Output: Verbatim text for the data penguins summary
      verbatimTextOutput("summary"),
      
      # Output: HTML table with the requested number of observations
      tableOutput("view")
      
    )
  )
)

# Here we define server logic to summarize and view the selected dataset penguins
server <- function(input, output) {
  datasetInput <- reactive({
    switch(input$dataset,
           "penguins" = penguins,
           "iris" = iris3,
           "mtcars" = mtcars)
  })
  
  
  output$caption <- renderText({
    input$caption
  })
  
  # Here we generate a summary of the dataset 
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(na.omit(dataset))
  })
  
  # This shows the first "n" observations 
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
}

shinyApp(ui, server)




## REACTIVE NUMBER 3, we look at different numeric variables and categorical variables based on different species, sex, islands

data <- na.omit(penguins)
data.numeric <- data[, c(3:6, 8)]
data.categorical <- data[, c(1,7)]


ui <- fluidPage(
  headerPanel("Penguin boxplots"),
  selectInput("ycol", 
              "Numeric Variable", 
              names(data.numeric),
              selected = names(data.numeric)[3]),
  selectInput("xcol",
              "Categorical Variable",
              names(data.categorical),
              selected = names(data.categorical)[2]),
  checkboxInput("split",
                "Split levels by island",
                value = TRUE),
  selectInput("species",
              "species Type",
              c("Unknown", levels(data$species))),
  mainPanel(
    plotOutput("plot1")
  ))


server <- function(input, output){
  output$plot1 <- renderPlot({
    par(mar = c(5, 4.1, 0, 1))
    
    if (input$species == "Unknown"){
      
      
      
      
      if (input$split) {
        ggplot(data, aes(x = .data[[input$xcol]], y = .data[[input$ycol]], fill = island)) +
          geom_boxplot(na.rm = TRUE) + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
          
          if (input$ycol == "Bill Length (mm)"){
            coord_cartesian(ylim = c(30, 60)) 
          } else if (input$ycol == "Bill Depth (mm)"){
            coord_cartesian(ylim = c(12.5, 21.5)) 
          } else if (input$ycol == "Flipper Length(mm)"){
            coord_cartesian(ylim = c(170, 232)) 
          } else if (input$ycol == "Body Mass (g)"){
            coord_cartesian(ylim = c(2500, 6500))
          } else if (input$ycol == "Year"){
            coord_cartesian(ylim = c(2006, 2009))
          } +
          xlab(input$xcol) +
          ylab(input$ycol) + 
          scale_x_discrete(labels = c("Female", "Male")) +
          scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
        
      } else {
        ggplot(data, aes(x = .data[[input$xcol]], y = .data[[input$ycol]])) +
          geom_boxplot(na.rm = TRUE) +scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
          if (input$ycol == "Bill Length (mm)"){
            coord_cartesian(ylim = c(30, 60)) 
          } else if (input$ycol == "Bill Depth (mm)"){
            coord_cartesian(ylim = c(12.5, 21.5)) 
          } else if (input$ycol == "Flipper Length(mm)"){
            coord_cartesian(ylim = c(170, 232)) 
          } else if (input$ycol == "Body Mass (g)"){
            coord_cartesian(ylim = c(2500, 6500))
          } else if (input$ycol == "Year"){
            coord_cartesian(ylim = c(2006, 2009))
          } +
          xlab(input$xcol) +
          ylab(input$ycol) 
      }
      
      
    } else {
      
      
      data <- data %>%
        filter(data$species == input$species)
      if (input$split) {
        ggplot(data, aes(x = .data[[input$xcol]], y = .data[[input$ycol]], fill = island)) +
          geom_boxplot(na.rm = TRUE) + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
          xlab(input$xcol) +
          ylab(input$ycol) + 
          scale_x_discrete(labels = c("Female", "Male")) 
        
      } else {
        ggplot(data, aes(x = .data[[input$xcol]], y = .data[[input$ycol]])) +
          geom_boxplot(na.rm = TRUE) +scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
          xlab(input$xcol) +
          ylab(input$ycol) 
      }
    }
    
    
  })
}


shinyApp(ui = ui, server = server)



## Reactive Number 4,  we run a series of plots against the dataset penguins that result in different data visualizations.


ui <- fluidPage(
  
  numericInput(inputId = "n", label = "number of plots", value = 5),
  
  
  
  mainPanel(
    plotOutput(outputId = "Oplts1"),
    plotOutput(outputId = "Oplts2")),
  plotOutput(outputId = "Oplts3"),
  plotOutput(outputId = "Oplts4"),
  plotOutput(outputId = "Oplts5")
)



server <- function(input, output) {
  output$Oplts1 <- renderPlot({
    
    ## PLOT 1
    penguins %>%
      count(species)
    
    head(penguins_raw)
    
    penguins %>%
      filter(!is.na(sex)) -> peng_orig
    summary(peng_orig)
    
    peng_orig %>%
      ggplot(aes(flipper_length_mm, bill_length_mm, color = sex, size = body_mass_g)) +
      geom_point(alpha = 0.5) +
      facet_wrap(~species)    
  })
  
  
  output$Oplts2 <- renderPlot({
    
    ## PLOT 2
    
    
    penguins %>%
      drop_na() %>%
      count(sex, species) %>%
      ggplot() + geom_col(aes(x = species, y = n, fill = species)) +
      geom_label(aes(x = species, y = n, label = n)) +
      facet_wrap(~sex) +
      labs(title = 'Penguins species ~ Gender') -> Oplts2
    Oplts2
    
  })
  
  
  output$Oplts3 <- renderPlot({
    
    
    ## PLOT 3
    
    ggplot(data = penguins, 
           aes(x = flipper_length_mm,
               y = body_mass_g)) +
      geom_point(aes(color = species, 
                     shape = species),
                 size = 2,
                 alpha = 0.5) +
      theme_minimal() +
      labs(title = 'Body Mass  ~ Flipper Length ') 
    
    
    
  })
  
  
  output$Oplts4 <- renderPlot({
    
    
    
    ## PLOT 4 
    
    ggplot(data = na.omit(penguins), 
           aes(x = flipper_length_mm,
               y = body_mass_g)) +
      geom_point(aes(color = island, 
                     shape = species),
                 size = 3,
                 alpha = 0.8) +
      theme_minimal() +
      scale_color_manual(values = c("darkorange","purple","cyan4")) +
      labs(title = "Penguin size, Body Mass ",
           subtitle = "Flipper length and body mass for each island",
           x = "Flipper length (mm)",
           y = "Body mass (g)",
           color = "Penguin island",
           shape = "Penguin species") 
    
    
  })
  
  
  
  output$Oplts5 <- renderPlot({
    
    
    
    ## PLOT 5 
    
    
    
    ggplot(data = penguins, 
           mapping = aes(x = year, y = body_mass_g, color = island)) +
      geom_line() -> Oplts5
    
    
    
    
    penguins_sum <- penguins %>%
      filter(!is.na(body_mass_g)) %>%
      group_by(island, year) %>%
      summarize(mean_body_mass_g = mean(body_mass_g))
    
    penguins_sum
    
    ggplot(data = penguins_sum, 
           mapping = aes(x = year, y = mean_body_mass_g, color = island)) +
      geom_line()
    
    
    
  })
  
  
  
}


shinyApp(ui = ui, server = server)




