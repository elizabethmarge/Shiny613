

library(tidyverse)
library(shiny)
library("palmerpenguins")
data = "penguins"
data = "penguins_raw"

## Part One, the introduction of the dataset, comparing with iris and mtcar, 
## but the aim of our projet is to introduce and operate palmerpenguins dataset,
## the iris and mtcars are only comparison. 


ui <- fluidPage(
  
  # App title ----
  titlePanel("penguins dataset with others"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Text for providing a caption ----
      # Note: Changes made to the caption in the textInput control
      # are updated in the output area immediately as you type
      textInput(inputId = "caption",
                label = "Caption:",
                value = "Data Summary"),
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "dataset",
                  label = "Choose a dataset:",
                  choices = c("penguins", "iris", "mtcars")),
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs",
                   label = "Number of observations to view:",
                   value = 10)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption", container = span)),
      
      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),
      
      # Output: HTML table with requested number of observations ----
      tableOutput("view")
      
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Return the requested dataset ----
  # By declaring datasetInput as a reactive expression we ensure
  # that:
  #
  # 1. It is only called when the inputs it depends on changes
  # 2. The computation and result are shared by all the callers,
  #    i.e. it only executes a single time
  datasetInput <- reactive({
    switch(input$dataset,
           "penguins" = penguins,
           "iris" = iris3,
           "mtcars" = mtcars)
  })
  
  
  output$caption <- renderText({
    input$caption
  })
  
  # Generate a summary of the dataset ----
  # The output$summary depends on the datasetInput reactive
  # expression, so will be re-executed whenever datasetInput is
  # invalidated, i.e. whenever the input$dataset changes
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(na.omit(dataset))
  })
  
  # Show the first "n" observations ----
  # The output$view depends on both the databaseInput reactive
  # expression and input$obs, so it will be re-executed whenever
  # input$dataset or input$obs is changed
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
}

shinyApp(ui, server)




## Part Two, different numeric variable based on different species, sex, islands


## REACTIVE NUMBER 2

data <- na.omit(penguins)
names(data)

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
              c("Unspecified", levels(data$species))),
  mainPanel(
    plotOutput("plot1")
  ))





server <- function(input, output){
  output$plot1 <- renderPlot({
    par(mar = c(5, 4.1, 0, 1))
    
    if (input$species == "Unspecified"){
      
      
      
      ## REACTIVE 
      
      
      if (input$split) {
        ggplot(data, aes(x = .data[[input$xcol]], y = .data[[input$ycol]], fill = island)) +
          geom_boxplot(na.rm = TRUE) +
          
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
          theme(text = element_text(size = 15))
        
      } else {
        ggplot(data, aes(x = .data[[input$xcol]], y = .data[[input$ycol]])) +
          geom_boxplot(na.rm = TRUE) +
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
          theme(text = element_text(size = 15))
      }
      
      
    } else {
      
      
      data <- data %>%
        filter(data$species == input$species)
      if (input$split) {
        ggplot(data, aes(x = .data[[input$xcol]], y = .data[[input$ycol]], fill = island)) +
          geom_boxplot(na.rm = TRUE) +
          xlab(input$xcol) +
          ylab(input$ycol) + 
          scale_x_discrete(labels = c("Female", "Male")) +
          theme(text = element_text(size = 15))
        
      } else {
        ggplot(data, aes(x = .data[[input$xcol]], y = .data[[input$ycol]])) +
          geom_boxplot(na.rm = TRUE) +
          xlab(input$xcol) +
          ylab(input$ycol) +
          theme(text = element_text(size = 15))
      }
    }
    
    
  })
}

## REACTIVE HISTOGRAM NUMBER 3 


shinyApp(ui = ui, server = server)



ui <- fluidPage(
  
  # App title ----
  titlePanel("Penguins!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # 
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    x    <- penguins_sum$mean_body_mass_g
    y <- penguins_sum$island
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "red", border = "white",
         xlab = "Body Mass of Penguins",
         y = "Island of Penguins",
         main = "Histogram of penguins")
    
  })
  
}
shinyApp(ui = ui, server = server)



## REACTIVE NUMBER 4 


ui <- fluidPage(
  titlePanel("Welcome to the World of Penguins <(^)"),
  textInput("name", "If you were a penguin, what would your name be?"),
  textOutput("greeting")
)

server <- function(input, output, session) {
  output$greeting <- renderText({
    paste("What a cool name for a penguin. Hi ", input$name, "!")
  })
}

shinyApp(ui = ui, server = server)



## I couldnt figure this out, maybe you can take a look? 



# Use a fluid Bootstrap layout
ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("Hello Penguins!"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("Penguins", "Species", 
                  choices=colnames(penguins)),
      #hr(),
      helpText("Data from Penguins dataset.")
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("Species Plot ")  
    )
    
  )
)

# Define a server for the Shiny app
server <- function(input, output) {
  
  # Fill in the spot we created for a plot
  output$penguinPlot <- renderPlot({
    
    # Render a barplot
    barplot(penguins[,input$penguins]*100, 
            main=input$penguins,
            ylab="Penguins",
            xlab="Year")
    
    
  })
}

shinyApp(ui = ui, server = server)




## extra plots we have 




## Part Three, different types of plots for penguins

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
  facet_wrap(~species)    -> Our_plots1


Our_plots1


## PLOT 2

ggplot(penguins, aes(x = island, y = species, color = species)) +
  geom_jitter(size = 3) 

penguins %>%
  drop_na() %>%
  count(sex, species) %>%
  ggplot() + geom_col(aes(x = species, y = n, fill = species)) +
  geom_label(aes(x = species, y = n, label = n)) +
  scale_fill_manual(values = c("darkorange","purple","cyan4")) +
  facet_wrap(~sex) +
  theme_minimal() +
  labs(title = 'Penguins species ~ Gender') -> Our_plots2
Our_plots2


## PLOT 3

ggplot(data = penguins, 
       aes(x = flipper_length_mm,
           y = body_mass_g)) +
  geom_point(aes(color = species, 
                 shape = species),
             size = 2,
             alpha = 0.5) +
  theme_minimal() +
  labs(title = 'Body Mass  ~ Flipper Length ') -> Our_plots3

Our_plots3


## PLOT 4 

ggplot(data = penguins, 
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
       shape = "Penguin species") -> Our_plots4

Our_plots4

ggplot(data = penguins, 
       mapping = aes(x = year, y = body_mass_g, color = island)) +
  geom_line() -> Our_plots5


## PLOT 5 

Our_plots5

penguins_sum <- penguins %>%
  filter(!is.na(body_mass_g)) %>%
  group_by(island, year) %>%
  summarize(mean_body_mass_g = mean(body_mass_g))

penguins_sum

ggplot(data = penguins_sum, 
       mapping = aes(x = year, y = mean_body_mass_g, color = island)) +
  geom_line()




