library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

# Load data
superstore_data <- read_excel("US Superstore data.xls", sheet = "Orders")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("US Superstore Data Explorer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          h2("Select Variables to Subset and Explore!"),
          
          # Choose (at least) two categorical variables they can subset from.
          # Categorical Variable 1.
          selectInput("CategoryFilter",
                      label = "Filter by Product Category",
                      choices = c("All", "Furniture", "Office Supplies", "Technology"), 
                      selected = "All"
          ),
          # Categorical Variable 2.
          selectInput("SegmentFilter",
                      label = "Filter by Customer Segment",
                      choices = c("All", "Consumer", "Corporate", "Home Office"), 
                      selected = "All"
        ),
        
        #Dynamic numeric variable sliders
        h2("Select numeric subsets."),
        
        selectInput("num_var_choice",
                    label = "Select Numeric Variable to Filter:",
                    choices = c("Sales", "Profit", "Quantity", "Discount"), 
                    selected = "Sales"
        ),
        
        uiOutput("dynamic_slider_ui"),
        
        selectInput("num_var_choice2",
                    label = "Select Numeric Variable to Filter:",
                    choices = c("Sales", "Profit", "Quantity", "Discount"), 
                    selected = "Profit"
        ),
        
        uiOutput("dynamic_slider2_ui"),
        
        # Create an action button
        actionButton("subset_data", "Subset the Data")
        
        ), 
        
        # Main Panel Code
        mainPanel(
          tabsetPanel(
            id = "tabs",
            
            #About tab
            tabPanel("About",
                     h2("About the App"),
                     p("Welcome to the Data Exploration App!"),
                     p("This Shiny app allows you to explore sales, profits, and other variables in a real U.S. Superstore dataset. 
                        Use the sidebar to subset the data based on consumer and product details, then explore the data across various settings."),
                     
                     # The data and it's source.
                     h3("About the Data"),
                     p("This dataset gives insights on online orders of a superstore in the U.S. It includes consumer orders from 2014 January - December 2017, with details about sales, profit, category, etc."),
                     p(HTML("Click here to access more details and information about the Superstore Dataset: 
                       <a href='https://www.kaggle.com/datasets/vivek468/superstore-dataset-final' target='_blank'>
                       US Superstore data on Kaggle</a>")),
                     # Image related to the dataset.
                     img(src = "superstore logo.png", 
                         height = "80px"),
                     
                     h3("How to use the App"),
                     tags$ul(
                       tags$li("Use the sidebar to subset the dataset."),
                       tags$li("Go to the 'Data Download' tab to see or download your subsetted data."),
                       tags$li("Explore the data for summaries and visualizations.")
                     )
            ),
            
            # Data Download Tab
            tabPanel("Data Download",
                     h2("Inspect and Export Your Subsets"),
                     p("After choosing your subsets in the sidebar, click 'Subset the Data' button to update this table."),
                     DT::dataTableOutput("superstore_table"),
                     downloadButton("download_data", "Download Subsetted Data"),
            ),
            
            #Data Exploration Tab
            tabPanel("Data Exploration",
                     h2("Obtain Numeric and Graphic Summaries"),
                     
                     # Choose to display categorical data summaries or numeric variable summaries.
                     radioButtons("explore_type", "Choose type of summary exploration:",
                                  choices = c("Categorical" = "cat",
                                              "Numeric" = "num")),
                     uiOutput("explore_inputs"),
                     
                     # Use subtabs for either tables or plots
                     tabsetPanel(
                       tabPanel("Table", tableOutput("exploration_table")),
                       tabPanel("Plot", plotOutput("exploration_plot"))
                     )
            )
          )
        )
      )
    )
    
# Define server logic 
server <- function(input, output, session) {

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
