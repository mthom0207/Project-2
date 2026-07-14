# Project-2

# US Superstore Data Explorer

This interactive R Shiny application allows users to dynamically explore, filter, and analyze operational trends, sales distributions, and performance metrics from the U.S. Superstore dataset. The user will be able to subset the data based on consumer and product details, then explore the data across various settings. The app will provide both summary tables and visual plots.

This dataset gives insights on online orders of a superstore in the U.S. It includes consumer orders from 2014-2018 (December 2017) with details about sales, profits, category, etc. 

## Core Features
* **Dynamic Subsetting:** Filter transactions by product categories, customer segments, and custom ranges for numeric variables (Sales, Profit, Quantity, and Discount).
* **Data Inspection & Export:** View active data subsets in an interactive data table and instantly download the customized filtered rows as a CSV file.
* **Flexible Data Exploration:** Toggle between automated numeric/contingency summary tables and dynamic reactive plots featuring custom coloring and faceting controls.

## Setup & Execution
1. Ensure the dataset `US Superstore data.xls` and your app file are in the same directory.
2. Ensure any app images are inside a local subfolder named `/www`.
3. Open `app.R` in RStudio and click **Run App**, or execute `shiny::runApp()` in your R console.
