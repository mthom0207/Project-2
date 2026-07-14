library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

# Load data
superstore_data <- read_excel("US Superstore data.xls", sheet = "Orders")

#create variable groups
category_vars <- c("Segment", "Category", "Sub-Category", "Region")
numeric_vars <- c("Sales", "Profit", "Quantity", "Discount")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("US Superstore Data Explorer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          h2("Select Variables to Subset and Explore!"),
          
          # Choose (at least) two categorical variables they can subset from.
          
          
          
          
          
          
          
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
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
