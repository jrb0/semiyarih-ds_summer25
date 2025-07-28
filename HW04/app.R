library(shiny)
library(ggplot2)
library(bslib)
library(thematic)
library(shinythemes)
library(fresh)
library(tidyverse)

create_theme(
  theme = "default",
  bs_vars_global(
    body_bg = "#FFF8BF"
  ),
  output_file = "www/mytheme.css"
)

mtcars_numeric <- mtcars

#boxplot requires variables as factors - normally would process this independently from the app.R file
mtcars <- mtcars %>%
  mutate(cyl = as.factor(cyl),
         vs = as.factor(vs),
         am = as.factor(am),
         gear = as.factor(gear),
         carb = as.factor(carb))

#create dataset for discreet vars
mtcars_discreet <- mtcars %>%
  select(where(is.factor))

#create dataset for continuous vars
mtcars_continuous <- mtcars %>%
  select(where(~ !is.factor(.)))

ui <- fluidPage(
  theme = 'mytheme.css',
  titlePanel(span("Homework Assignment", style = "color: red;")),
  h4("Jonathan Bachrach"),
  sidebarLayout(
    sidebarPanel(
      varSelectInput("discreet", "Discreet Variable", data = mtcars_discreet),
      varSelectInput("continuous", "Continuous Variable", data = mtcars_continuous)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data", 
                 h4("Dataset: mtcars"),
                 tableOutput("data")),
        
        tabPanel("Summary",
                 h4("Discreet"),
                 verbatimTextOutput("summary_discreet"),
                 h4("Continuous"),
                 verbatimTextOutput("summary_continuous")
                 ),
        
        tabPanel("Boxplot",
                 plotOutput("boxplot")),
        
        tabPanel("Bar",
                 h4("Frequency - Discreet Variable"),
                 plotOutput("bar")),
        
        tabPanel("Histogram",
                 h4("Frequency - Continuous Variable"),
                 plotOutput("histogram"))
        
      )
    )
  )
)


server <- function(input, output) {
  
  output$data <- renderTable({
    mtcars |>
      select(!!input$discreet, !!input$continuous)
  })
  
  output$summary_discreet <- renderPrint({
    mtcars_numeric %>%
      summarize(
        Mean = mean(!!input$discreet),
        Median = median(!!input$discreet),
        Min = min(!!input$discreet),
        Max = max(!!input$discreet),
        Standard_Deviation = sd(!!input$discreet)
      )
  })
  
  output$summary_continuous <- renderPrint({
    mtcars_continuous %>%
      summarize(
        Mean = mean(!!input$continuous),
        Median = median(!!input$continuous),
        Min = min(!!input$continuous),
        Max = max(!!input$continuous),
        Standard_Deviation = sd(!!input$continuous)
      )
  })
  
  output$boxplot <- renderPlot({
    ggplot(mtcars, aes(x = !!input$discreet, y = !!input$continuous)) +
      geom_boxplot()
  })
  
  output$bar <- renderPlot({
    ggplot(mtcars, aes(x = !!input$discreet)) +
      geom_bar()
  })
  
  output$histogram <- renderPlot({
    ggplot(mtcars, aes(x = !!input$continuous)) +
      geom_histogram()
  })
}
  
# Run the application 
shinyApp(ui = ui, server = server)
