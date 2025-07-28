library(shiny)
library(ggplot2)
library(bslib)
library(thematic)
library(shinythemes)
library(fresh)

create_theme(
  theme = "default",
  bs_vars_color(
    gray_base = "green"
  ),
  bs_vars_wells(
    bg = "#90ee90",
    border = "#552D42"
  ),
  bs_vars_global(
    body_bg = "#e5ffe5"
  ),
  bs_vars_input(
    color = "#5d3954",
    border_radius = "20px"
  ),
  #
  output_file = "www/mytheme.css"
)

ui <- fluidPage(
  theme = 'mytheme.css',
  h1("Homework Assignment"),
  h4("Jonathan Bachrach"),
  sidebarLayout(
    sidebarPanel(
      varSelectInput("discreet", "Variable 1", data = mtcars),
      varSelectInput("continuous", "Variable 2", data = mtcars)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data", plotOutput("hist")),
        tabPanel("Summary"),
        tabPanel("Boxplot"),
        tabPanel("Bar"),
        tabPanel("Summary")
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
