library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(readxl)         
library(DT)
library(shinycssloaders)
library(tidyr)

# Load data
superstore_data <- read_excel("US Superstore data.xls", sheet = "Orders")

# Define global variable lookup vectors so they can be referenced inside the server
category_vars <- c("Category", "Segment", "Sub-Category", "Region")
numeric_vars  <- c("Sales", "Profit", "Quantity", "Discount")

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
                       <a href='https://www.kaggle.com/datasets/juhi1994/superstore/data' target='_blank'>
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
                       tabPanel("Plot", shinycssloaders::withSpinner(plotOutput("exploration_plot"), type = 6, color = "#0275d8"))                     )
            )
          )
        )
      )
    )
    
# Define server logic 
server <- function(input, output, session) {
  
  # Prevents users from selecting the same column variable in both inputs
  observeEvent(input$num_var_choice, {
    updateSelectizeInput(session, "num_var_choice2",
                         choices = setdiff(numeric_vars, input$num_var_choice),
                         selected = input$num_var_choice2)
  })
  
  observeEvent(input$num_var_choice2, {
    updateSelectizeInput(session, "num_var_choice",
                         choices = setdiff(numeric_vars, input$num_var_choice2),
                         selected = input$num_var_choice)
  })
  
  output$dynamic_slider_ui <- renderUI({
    req(input$num_var_choice)
    sliderInput("num1_range",
                label = paste("Select range for", input$num_var_choice, ":"),
                min = min(superstore_data[[input$num_var_choice]], na.rm = TRUE),
                max = max(superstore_data[[input$num_var_choice]], na.rm = TRUE),
                value = c(min(superstore_data[[input$num_var_choice]], na.rm = TRUE),
                          max(superstore_data[[input$num_var_choice]], na.rm = TRUE)))
  })
  
  output$dynamic_slider2_ui <- renderUI({
    req(input$num_var_choice2)
    sliderInput("num2_range",
                label = paste("Select range for", input$num_var_choice2, ":"),
                min = min(superstore_data[[input$num_var_choice2]], na.rm = TRUE),
                max = max(superstore_data[[input$num_var_choice2]], na.rm = TRUE),
                value = c(min(superstore_data[[input$num_var_choice2]], na.rm = TRUE),
                          max(superstore_data[[input$num_var_choice2]], na.rm = TRUE)))
  })
  
  # Initialize reactive values with full data baseline so it is not blank on launch
  subsetted_data <- reactiveValues(data = superstore_data)
  
  # Action button subset data logic
  observeEvent(input$subset_data, {
    req(input$CategoryFilter, input$SegmentFilter)
    
    temp_df <- superstore_data
    
    if (input$CategoryFilter != "All") {
      temp_df <- temp_df %>% filter(Category == input$CategoryFilter)
    }
    if (input$SegmentFilter != "All") {
      temp_df <- temp_df %>% filter(Segment == input$SegmentFilter)
    }
    
    # Safely apply numerical filters if dynamic UI is loaded
    if (!is.null(input$num1_range) && !is.null(input$num2_range)) {
      temp_df <- temp_df %>%
        filter(
          .data[[input$num_var_choice]] >= input$num1_range[1],
          .data[[input$num_var_choice]] <= input$num1_range[2],
          .data[[input$num_var_choice2]] >= input$num2_range[1],
          .data[[input$num_var_choice2]] <= input$num2_range[2]
        )
    }
    
    subsetted_data$data <- temp_df
  }, ignoreNULL = FALSE)
  
  # Display the data table
  output$superstore_table <- DT::renderDataTable({
    req(subsetted_data$data)
    DT::datatable(subsetted_data$data, options = list(pageLength = 5, scrollX = TRUE))
  })
  
  # Data Download tab
  output$download_data <- downloadHandler(
    filename = function() { paste0("superstore_subsetted.csv") },
    content = function(file) { write.csv(subsetted_data$data, file, row.names = FALSE) }
  )
  
  # Variable selection layout with explicit custom modifier controls (color, faceting)
  output$explore_inputs <- renderUI({
    req(subsetted_data$data)
    
    if (input$explore_type == "cat") {
      tagList(
        fluidRow(
          column(6, selectInput("cat_var", "Select Primary Categorical Variable:", choices = category_vars)),
          column(6, selectInput("cat_var2", "Select Modifying/Grouping Variable:", choices = c(None = ".", category_vars)))
        )
      )
    } else {
      tagList(
        fluidRow(
          column(4, selectInput("num_var", "Select Numeric Variable:", choices = numeric_vars)),
          column(4, selectInput("num_var2", "Optional 2nd Numeric Variable (Scatter):", choices = c(None = ".", numeric_vars))),
          column(4, selectInput("cat_var", "Grouping/Modifier (Categorical):", choices = c(None = ".", category_vars)))
        ),
        fluidRow(
          column(6, selectInput("facet_var", "Facet Plots By:", choices = c(None = ".", category_vars)))
        )
      )
    }
  })
  
  # Exploration Tables and Summaries
  output$exploration_table <- renderTable({
    req(subsetted_data$data)
    # Error prevention rule check
    validate(need(nrow(subsetted_data$data) > 0, "No data available with current subset settings."))
    
    if (input$explore_type == "cat") {
      req(input$cat_var)
      if (input$cat_var2 %in% c(".", "None")) {
        tbl <- table(subsetted_data$data[[input$cat_var]])
        return(data.frame(Category = names(tbl), Frequency = as.vector(tbl)))
      } else {
        subsetted_data$data %>%
          count(.data[[input$cat_var]], .data[[input$cat_var2]]) %>%
          pivot_wider(names_from = input$cat_var2, values_from = n, values_fill = 0) 
      }
    } else if (input$explore_type == "num") {
      req(input$num_var)
      if (is.null(input$cat_var) || input$cat_var == ".") {
        subsetted_data$data %>%
          summarise(Mean = mean(.data[[input$num_var]], na.rm = TRUE),
                    Median = median(.data[[input$num_var]], na.rm = TRUE),
                    SD = sd(.data[[input$num_var]], na.rm = TRUE))
      } else { 
        subsetted_data$data %>%
          group_by(.data[[input$cat_var]]) %>%
          summarise(Mean = mean(.data[[input$num_var]], na.rm = TRUE),
                    Median = median(.data[[input$num_var]], na.rm = TRUE),
                    SD = sd(.data[[input$num_var]], na.rm = TRUE))
      }
    }
  })
  
  # Exploration Plots with validation and advanced faceting logic
  output$exploration_plot <- renderPlot({
    req(subsetted_data$data)
    validate(need(nrow(subsetted_data$data) > 0, "No data matching criteria to generate plots."))
    
    withProgress(message = "Rendering plot...", value = 0, {
      
      if (input$explore_type == "cat") {
        req(input$cat_var)
        if (input$cat_var2 %in% c(".", "None")) {
          p <- ggplot(subsetted_data$data, aes(x = .data[[input$cat_var]], fill = .data[[input$cat_var]])) +
            geom_bar() + labs(title = paste("Orders Count by", input$cat_var), y = "Count")
        } else { 
          p <- ggplot(subsetted_data$data, aes(x = .data[[input$cat_var]], fill = .data[[input$cat_var2]])) +
            geom_bar(position = "dodge") + labs(title = paste("Orders by", input$cat_var, "and", input$cat_var2))
        }
        return(p)
        
      } else {
        req(input$num_var)
        # 1-Variable Numeric Layout (Histogram / Boxplot)
        if (is.null(input$num_var2) || input$num_var2 %in% c(".", "None")) {
          if (is.null(input$cat_var) || input$cat_var %in% c(".", "None")) {
            p <- ggplot(subsetted_data$data, aes(x = .data[[input$num_var]])) +
              geom_histogram(fill = "darkcyan", color = "black", bins = 30) +
              labs(title = paste("Histogram Distribution of", input$num_var))
          } else { 
            p <- ggplot(subsetted_data$data, aes(x = .data[[input$cat_var]], y = .data[[input$num_var]], fill = .data[[input$cat_var]])) +
              geom_boxplot() + coord_flip() + labs(title = paste(input$num_var, "Distribution Across", input$cat_var))
          }
          # 2-Variable Scatter plots
        } else { 
          if (is.null(input$cat_var) || input$cat_var %in% c(".", "None")) {
            p <- ggplot(subsetted_data$data, aes(x = .data[[input$num_var]], y = .data[[input$num_var2]])) +
              geom_point(color = "darkcyan", alpha = 0.6) + labs(title = paste(input$num_var, "vs", input$num_var2))
          } else {
            p <- ggplot(subsetted_data$data, aes(x = .data[[input$num_var]], y = .data[[input$num_var2]], color = .data[[input$cat_var]])) +
              geom_point(alpha = 0.6) + labs(title = paste(input$num_var, "vs", input$num_var2, "Colored by", input$cat_var))
          }
        }
        
        # Apply structural user-selected Faceting modifiers safely if called
        if (!is.null(input$facet_var) && input$facet_var != ".") {
          p <- p + facet_wrap(as.formula(paste("~", paste0("`", input$facet_var, "`"))))
        }
        return(p)
      }
    })
  })
}

shinyApp(ui = ui, server = server)