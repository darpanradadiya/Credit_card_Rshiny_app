###############################################
# ALY6050 : Assignment-3
# Author: Darpan Radadiya
# Date: March 23,2025
# Project - Individual R shiny Application
# Topic : Customer segment analysis of credit card transactions.

# Load required libraries
library(shiny)
library(shinydashboard)  
library(shinydashboardPlus)  
library(dplyr)
library(ggplot2)
library(readr)
library(plotly)
library(DT)  
library(lubridate)  
library(shinyWidgets)  

df <- read_csv("/Users/darpanradadiya/Downloads/credit_card_transaction_flow.csv", 
               col_types = cols(
                 Birthdate = col_date(format = "%d-%m-%Y"),
                 `Date` = col_date(format = "%d-%m-%Y")
               ))
df$Gender[is.na(df$Gender)] <- "Unknown"
df$`Transaction Amount` <- as.numeric(df$`Transaction Amount`)
df$Category <- as.factor(df$Category)
df$Gender <- as.factor(df$Gender)

df <- df %>%
  mutate(Age = as.numeric(difftime(as.Date("2025-03-23"), Birthdate, units = "days")) / 365.25)


travel_colors <- c(
  "Clothing" = "#F9C74F",      # Sandy Beige 
  "Cosmetic" = "#F28C38",      # Sunset Orange
  "Electronics" = "#2C3E50",   # Deep Ocean Blue
  "Market" = "#E07A5F",        # Terracotta 
  "Restaurant" = "#F4A261",    # Warm Peach 
  "Travel" = "#468FAF",        # Sky Blue 
  "M" = "#468FAF",            # Sky Blue
  "F" = "#F28C38",            # Sunset Orange
  "Unknown" = "#6B7280",       # Neutral Gray
  "High Spenders" = "#34C759", # Emerald Green
  "Medium Spenders" = "#FFD60A", # Sunflower Yellow
  "Low Spenders" = "#FF3B30",  # Coral Red
  "Under 25" = "#A3BFFA",      # Light Blue 
  "25-34" = "#7F9CF5",        # Medium Blue
  "35-44" = "#5A7EF5",        # Slightly Darker Blue
  "45-54" = "#4267B2",        # Dark Blue
  "55+" = "#2A4E8C"           # Deep Blue
)

# UI with a professional dashboard layout
ui <- dashboardPage(
  skin = "blue", 
  dashboardHeader(
    title = div(
      style = "display: flex; justify-content: left; align-items: right; padding: 10px 20px; width: 100%;",  
      div(
        style = "display: flex; align-items: left;",
        icon("plane", style = "font-size: 28px; color: #F9C74F; margin-right: 0px;"),  
        div(
          h3("Customer Segmentation Analysis", 
             style = "color: #FFFFFF; font-size: 20px; font-family: 'Roboto', sans-serif; margin: 0; text-shadow: 1px 1px 2px rgba(0,0,0,0.3);"),
          h5("Unveiling Spending Patterns to Optimize Reward Programs", 
             style = "color: #F9C74F; font-size: 12px; font-family: 'Roboto', sans-serif; margin: 0; text-shadow: 1px 1px 2px rgba(0,0,0,0.3);")  
        )
      )
    ),
    titleWidth = 600
  ),
  dashboardSidebar(
    width = 260,
    sidebarMenu(
      menuItem("Filters", icon = icon("sliders-h"), startExpanded = TRUE,
               selectInput("category", "Select Category:", choices = unique(df$Category)),
               numericInput("min_spend", "Minimum Spending Amount:", value = 0),
               numericInput("max_spend", "Maximum Spending Amount:", value = max(df$`Transaction Amount`)),
               dateRangeInput("date_range", "Select Date Range:", 
                              start = min(df$Date), end = max(df$Date))),
      menuItem("Demographic Filters", icon = icon("users"), startExpanded = TRUE,
               selectInput("spending_tier", "Select Spending Tier:", 
                           choices = c("All", "High Spenders", "Medium Spenders", "Low Spenders"), selected = "All"),
               selectInput("age_group", "Select Age Group:", 
                           choices = c("All", "Under 25", "25-34", "35-44", "45-54", "55+"), selected = "All"),
               selectInput("gender", "Select Gender:", 
                           choices = c("All", levels(df$Gender)), selected = "All")),
      downloadButton("download_segments", "Download Customer Segments", style = "margin-top: 10px; width: 100%;")
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .box { border-top: 3px solid #468FAF; background: linear-gradient(to bottom, #f5f5f5, #e0e0e0); }
      h3 { color: #468FAF; }
      h4 { color: #666; }
      .shiny-text-output { font-size: 16px; color: #666; }
      .no-data-message { font-size: 16px; color: #FF3B30; text-align: center; }
      .welcome-message { font-size: 16px; color: #666; background: linear-gradient(to bottom, #f5f5f5, #e0e0e0); padding: 10px; border-radius: 5px; }
      .plot-container { margin-bottom: 10px; }
      .main-header { 
        background: linear-gradient(to right, #2C3E50, #468FAF); 
        box-shadow: 0 2px 4px rgba(0,0,0,0.2);
        position: fixed;  /* Make header fixed */
        top: 0;
        width: 100%;
        z-index: 1000; 
      }
      .main-header .logo { 
        padding: 10;
        width: 90%;
      }
      .content-wrapper, .main-sidebar { 
        padding-top: 50px;  
      }
    "))),
    fluidRow(
      box(
        width = 12,
        h3("Key Insights"),
        verbatimTextOutput("key_insights"),
        style = "padding: 10px; border-radius: 5px; margin-bottom: 20px;"
      )
    ),
    tabBox(
      width = 12,
      tabPanel("Overview",
               h3("Travel Dominates: Key Insights"),
               div(class = "welcome-message",
                   p("Welcome to the Customer Segmentation Analysis! This dashboard helps you understand spending patterns across categories, with a focus on Travel, which dominates at 58.4% of total spend. Use the filters to explore demographic insights, temporal trends, and key customers/merchants to optimize reward programs and marketing campaigns.")),
               fluidRow(
                 column(6, plotlyOutput("category_dominance_plot", height = "350px")),
                 column(6, DTOutput("category_summary_table"))
               ),
               fluidRow(
                 column(6, plotlyOutput("gender_distribution_plot", height = "350px")),
                 column(6, plotlyOutput("age_distribution_plot", height = "350px"))
               )),
      tabPanel("Demographics",
               h3("Demographic Insights"),
               fluidRow(
                 column(6, div(class = "plot-container", plotlyOutput("spending_distribution", height = "300px"))),
                 column(6, div(class = "plot-container", plotlyOutput("spending_tier_plot", height = "300px")))
               ),
               fluidRow(
                 column(6, div(class = "plot-container", plotlyOutput("age_group_plot", height = "300px"))),
                 column(6, div(class = "plot-container", plotlyOutput("gender_combined_plot", height = "300px")))
               ),
               fluidRow(
                 column(6, div(class = "plot-container", plotlyOutput("spending_frequency_plot", height = "300px"))),
                 column(6, div(class = "plot-container", plotlyOutput("transaction_size_distribution_plot", height = "300px")))
               )),
      tabPanel("Trends",
               h3("Temporal Trends"),
               fluidRow(
                 column(12, plotlyOutput("spending_trend_plot", height = "400px"))
               ),
               fluidRow(
                 column(12, plotlyOutput("day_of_week_patterns_plot", height = "400px"))
               )),
      tabPanel("Key Customers & Merchants",
               h3("Top Spenders and Merchants"),
               fluidRow(
                 column(6, DTOutput("top_spenders")),
                 column(6, DTOutput("top_merchants"))
               ),
               fluidRow(
                 column(12, 
                        h3("Top Merchants for High Spenders in Travel"),
                        plotlyOutput("top_merchants_high_spenders_travel", height = "400px"))
               )),
      tabPanel("Recommendations",
               h3("Actionable Recommendations"),
               div(class = "welcome-message",
                   p("Based on the analysis, here are actionable recommendations to optimize reward programs and marketing campaigns:")),
               fluidRow(
                 column(12,
                        tags$ul(
                          tags$li("Focus marketing efforts on the Travel category, which dominates at 58.4% of total spend. Offer exclusive travel-related rewards to high spenders."),
                          tags$li("Target the 55+ age group, which represents the largest segment with tailored promotions for Travel and Electronics."),
                          tags$li("Increase transaction values in lower-performing categories like Restaurant and Cosmetic by offering incentives for larger purchases."),
                          tags$li("Leverage peak months (e.g., July) for Travel promotions, as transaction volume increases from October to July."),
                          tags$li("Partner with top merchants for Travel promotions, as they are popular among high spenders."),
                          tags$li("Maintain consistent marketing efforts throughout the week, as spending patterns are stable across days.")
                        ))
               ))
    )
  )
)

#  the server logic
server <- function(input, output) {
  
 
  filtered_data <- reactive({
    data <- df %>%
      filter(Category == input$category,
             `Transaction Amount` >= input$min_spend,
             `Transaction Amount` <= input$max_spend,
             Date >= input$date_range[1],
             Date <= input$date_range[2])
    
    # Calculate spending tiers
    total_spending <- data %>%
      group_by(`Customer ID`) %>%
      summarise(Total_Spending = sum(`Transaction Amount`), .groups = "drop")
    
    total_spending <- total_spending %>%
      mutate(Spending_Tier = case_when(
        Total_Spending >= quantile(Total_Spending, 0.8) ~ "High Spenders",
        Total_Spending >= quantile(Total_Spending, 0.4) ~ "Medium Spenders",
        TRUE ~ "Low Spenders"
      ))
    
    # Join spending tiers back to the data
    data <- data %>%
      left_join(total_spending %>% select(`Customer ID`, Spending_Tier), by = "Customer ID")
    
    # Calculate age and age groups
    data <- data %>%
      mutate(Age = as.numeric(difftime(as.Date("2025-03-23"), Birthdate, units = "days")) / 365.25,
             Age_Group = case_when(
               Age < 25 ~ "Under 25",
               Age < 35 ~ "25-34",
               Age < 45 ~ "35-44",
               Age < 55 ~ "45-54",
               TRUE ~ "55+"
             ))
    
    # Apply spending tier filter
    if (input$spending_tier != "All") {
      data <- data %>% filter(Spending_Tier == input$spending_tier)
    }
    
    # Apply age group filter
    if (input$age_group != "All") {
      data <- data %>% filter(Age_Group == input$age_group)
    }
    
    # Apply gender filter
    if (input$gender != "All") {
      data <- data %>% filter(Gender == input$gender)
    }
    
    data
  })
  
  output$key_insights <- renderText({
    data <- filtered_data()
    if (nrow(data) == 0) {
      return("No transactions available for this selection.")
    }
    else{
    total_transactions <- nrow(data)
    avg_transaction <- mean(data$`Transaction Amount`, na.rm = TRUE)
    total_spending <- sum(data$`Transaction Amount`, na.rm = TRUE)
    
    top_spender <- data %>%
      group_by(`Customer ID`, Name, Surname) %>%
      summarise(Total_Spending = sum(`Transaction Amount`), .groups = "drop") %>%
      arrange(desc(Total_Spending)) %>%
      slice_head(n = 1)
    
    top_merchant <- data %>%
      group_by(`Merchant Name`) %>%
      summarise(Total_Spending = sum(`Transaction Amount`), .groups = "drop") %>%
      arrange(desc(Total_Spending)) %>%
      slice_head(n = 1)
    
    paste("Key Insights:\n",
          "Total Transactions: ", total_transactions, "\n",
          "Average Transaction Amount: $", round(avg_transaction, 2), "\n",
          "Total Spending: $", round(total_spending, 2), "\n",
          "Top Spender: ", top_spender$Name, " ", top_spender$Surname, " ($", round(top_spender$Total_Spending, 2), ")\n",
          "Top Merchant: ", top_merchant$`Merchant Name`, " ($", round(top_merchant$Total_Spending, 2), ")\n")}
  })
  
  # Overview: Category dominance plot
  output$category_dominance_plot <- renderPlotly({
    category_summary <- df %>%
      group_by(Category) %>%
      summarise(Transactions = n(), .groups = "drop")
    
    p <- ggplot(category_summary, aes(x = reorder(Category, Transactions), y = Transactions, fill = Category)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Transactions), hjust = -0.2) +
      coord_flip() +
      labs(title = "Travel Dominates Transactions Across Categories",
           subtitle = "Which categories drive the most transactions?",
           x = "Category", y = "Number of Transactions") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.major = element_line(color = "grey80"),
            panel.grid.minor = element_line(color = "grey90")) +
      scale_fill_manual(values = travel_colors[names(travel_colors) %in% category_summary$Category])
    ggplotly(p, tooltip = c("Category", "Transactions"))
  })
  
  # Overview: Category summary table
  output$category_summary_table <- renderDT({
    category_summary <- df %>%
      group_by(Category) %>%
      summarise(
        Transactions = n(),
        Total_Spending = sum(`Transaction Amount`),
        .groups = "drop"
      ) %>%
      arrange(desc(Transactions))
    
    datatable(category_summary, options = list(pageLength = 5, searching = TRUE, ordering = TRUE))
  })
  
  # Overview: Gender distribution bar plot 
  output$gender_distribution_plot <- renderPlotly({
    gender_summary <- df %>%
      group_by(Gender) %>%
      summarise(Transactions = n(), .groups = "drop") %>%
      mutate(Percentage = Transactions / sum(Transactions) * 100)
    
    p <- ggplot(gender_summary, aes(x = Gender, y = Transactions, fill = Gender)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = -0.5) +
      labs(title = "Gender Distribution of Transactions",
           subtitle = "How are transactions distributed by gender?",
           x = "Gender", y = "Number of Transactions") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.major = element_line(color = "grey80"),
            panel.grid.minor = element_line(color = "grey90")) +
      scale_fill_manual(values = travel_colors[names(travel_colors) %in% gender_summary$Gender])
    ggplotly(p, tooltip = c("Gender", "Transactions", "Percentage"))
  })
  
  # Overview: Age distribution bar chart
  output$age_distribution_plot <- renderPlotly({
    age_summary <- df %>%
      mutate(Age_Group = case_when(
        Age < 25 ~ "Under 25",
        Age < 35 ~ "25-34",
        Age < 45 ~ "35-44",
        Age < 55 ~ "45-54",
        TRUE ~ "55+"
      )) %>%
      group_by(Age_Group) %>%
      summarise(Transactions = n(), .groups = "drop") %>%
      mutate(Percentage = Transactions / sum(Transactions) * 100)
    
    p <- ggplot(age_summary, aes(x = Age_Group, y = Transactions, fill = Age_Group)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = -0.5) +
      labs(title = "Age Group Distribution of Transactions",
           subtitle = "Which age groups are most active?",
           x = "Age Group", y = "Number of Transactions") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.major = element_line(color = "grey80"),
            panel.grid.minor = element_line(color = "grey90")) +
      scale_fill_manual(values = travel_colors[names(travel_colors) %in% age_summary$Age_Group])
    ggplotly(p, tooltip = c("Age_Group", "Transactions", "Percentage"))
  })
  
  # Interactive histogram using plotly
  output$spending_distribution <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) {
      return(NULL)
    }
    
    p <- ggplot(data, aes(x = `Transaction Amount`, fill = Category)) +
      geom_histogram(binwidth = 50, color = "black") +
      labs(title = paste("Spending Distribution for", input$category),
           subtitle = "What is the distribution of transaction amounts?",
           x = "Transaction Amount ($)", y = "Count") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.major = element_line(color = "grey80"),
            panel.grid.minor = element_line(color = "grey90")) +
      scale_fill_manual(values = travel_colors[names(travel_colors) %in% data$Category])
    ggplotly(p, tooltip = c("x", "count"))
  })
  
  # Bar plot of customer spending tiers with data labels
  output$spending_tier_plot <- renderPlotly({
    total_spending <- filtered_data()
    if (nrow(total_spending) == 0) {
      return(NULL)
    }
    
    total_spending <- total_spending %>%
      group_by(`Customer ID`) %>%
      summarise(Total_Spending = sum(`Transaction Amount`), .groups = "drop")
    
    total_spending <- total_spending %>%
      mutate(Spending_Tier = case_when(
        Total_Spending >= quantile(Total_Spending, 0.8) ~ "High Spenders",
        Total_Spending >= quantile(Total_Spending, 0.4) ~ "Medium Spenders",
        TRUE ~ "Low Spenders"
      ))
    
    tier_summary <- total_spending %>%
      group_by(Spending_Tier) %>%
      summarise(Customer_Count = n(), .groups = "drop")
    
    p <- ggplot(tier_summary, aes(x = Spending_Tier, y = Customer_Count, fill = Spending_Tier)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Customer_Count), vjust = -0.5) +
      labs(title = "Customer Distribution by Spending Tier",
           subtitle = "How are customers distributed across spending levels?",
           x = "Spending Tier", y = "Number of Customers") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.major = element_line(color = "grey80"),
            panel.grid.minor = element_line(color = "grey90")) +
      scale_fill_manual(values = travel_colors[names(travel_colors) %in% tier_summary$Spending_Tier])
    ggplotly(p, tooltip = c("Spending_Tier", "Customer_Count"))
  })
  
  # Bar plot for age group distribution with data labels
  output$age_group_plot <- renderPlotly({
    age_summary <- filtered_data()
    if (nrow(age_summary) == 0) {
      return(NULL)
    }
    
    age_summary <- age_summary %>%
      group_by(Age_Group) %>%
      summarise(Customer_Count = n_distinct(`Customer ID`), .groups = "drop")
    
    p <- ggplot(age_summary, aes(x = Age_Group, y = Customer_Count, fill = Age_Group)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Customer_Count), vjust = -0.5) +
      labs(title = "Customer Distribution by Age Group",
           subtitle = "Which age groups are most active in this category?",
           x = "Age Group", y = "Number of Customers") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.major = element_line(color = "grey80"),
            panel.grid.minor = element_line(color = "grey90")) +
      scale_fill_manual(values = travel_colors[names(travel_colors) %in% age_summary$Age_Group])
    ggplotly(p, tooltip = c("Age_Group", "Customer_Count"))
  })
  
  # Combined bar plot for gender distribution and total spending by gender
  output$gender_combined_plot <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) {
      return(NULL)
    }
    
    gender_summary <- data %>%
      group_by(Gender) %>%
      summarise(Total_Spending = sum(`Transaction Amount`), Transactions = n(), .groups = "drop") %>%
      mutate(Percentage = Transactions / sum(Transactions) * 100)
    
    p <- ggplot(gender_summary, aes(x = Gender)) +
      geom_bar(aes(y = Total_Spending, fill = Gender), stat = "identity", position = "dodge") +
      geom_text(aes(y = Total_Spending, label = sprintf("%.1f%%", Percentage)), vjust = -0.5) +
      labs(title = "Total Spending and Distribution by Gender",
           subtitle = "How does spending vary by gender? (Percentages show transaction distribution)",
           x = "Gender", y = "Total Spending ($)") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.major = element_line(color = "grey80"),
            panel.grid.minor = element_line(color = "grey90")) +
      scale_fill_manual(values = travel_colors[names(travel_colors) %in% gender_summary$Gender])
    ggplotly(p, tooltip = c("Gender", "Total_Spending", "Percentage"))
  })
  
  # Box plot for spending frequency by age group
  output$spending_frequency_plot <- renderPlotly({
    frequency_data <- filtered_data()
    if (nrow(frequency_data) == 0) {
      return(NULL)
    }
    
    frequency_data <- frequency_data %>%
      group_by(`Customer ID`, Age_Group, Category, Gender, Spending_Tier) %>%
      summarise(Transactions = n(), .groups = "drop")
    
    p <- ggplot(frequency_data, aes(x = Age_Group, y = Transactions, fill = Age_Group)) +
      geom_boxplot() +
      labs(title = "Spending Frequency by Age Group",
           subtitle = "How often do customers in each age group transact?",
           x = "Age Group", y = "Number of Transactions per Customer") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.major = element_line(color = "grey80"),
            panel.grid.minor = element_line(color = "grey90")) +
      scale_fill_manual(values = travel_colors[names(travel_colors) %in% frequency_data$Age_Group])
    ggplotly(p, tooltip = c("Age_Group", "Transactions"))
  })
  
  # Stacked bar plot for transaction size distribution by category
  output$transaction_size_distribution_plot <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) {
      return(NULL)
    }
    
    data <- data %>%
      mutate(Transaction_Size = case_when(
        `Transaction Amount` < 250 ~ "$0-$249",
        `Transaction Amount` < 500 ~ "$250-$499",
        `Transaction Amount` < 1000 ~ "$500-$999",
        `Transaction Amount` < 2000 ~ "$1000-$1999",
        TRUE ~ "$2000+"
      ))
    
    size_summary <- data %>%
      group_by(Transaction_Size, Category) %>%
      summarise(Transactions = n(), .groups = "drop")
    
    p <- ggplot(size_summary, aes(x = Transaction_Size, y = Transactions, fill = Category)) +
      geom_bar(stat = "identity") +
      labs(title = "Transaction Size Distribution by Category",
           subtitle = "How do transaction sizes vary across categories?",
           x = "Transaction Size", y = "Number of Transactions") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.major = element_line(color = "grey80"),
            panel.grid.minor = element_line(color = "grey90"),
            legend.position = "bottom") +
      scale_fill_manual(values = travel_colors[names(travel_colors) %in% size_summary$Category])
    ggplotly(p, tooltip = c("Transaction_Size", "Transactions", "Category"))
  })
  
  # Line plot for spending trends over time with category comparison and seasonal highlights
  output$spending_trend_plot <- renderPlotly({
    trend_data <- filtered_data()
    if (nrow(trend_data) == 0) {
      return(NULL)
    }
    
    trend_data <- trend_data %>%
      mutate(Month = floor_date(Date, "month")) %>%
      group_by(Month, Category) %>%
      summarise(Total_Spending = sum(`Transaction Amount`), .groups = "drop")
    
    # Identify peak months
    peak_months <- trend_data %>%
      group_by(Month) %>%
      summarise(Total_Spending = sum(Total_Spending), .groups = "drop") %>%
      arrange(desc(Total_Spending)) %>%
      slice_head(n = 2) %>%
      pull(Month)
    
    p <- ggplot(trend_data, aes(x = Month, y = Total_Spending, color = Category)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      geom_rect(aes(xmin = input$date_range[1], xmax = input$date_range[2], ymin = -Inf, ymax = Inf), 
                fill = "grey", alpha = 0.2) +
      geom_vline(xintercept = as.numeric(peak_months), linetype = "dashed", color = "red", linewidth = 0.5) +
      labs(title = "Spending Trends Over Time by Category",
           subtitle = "How does spending in Travel compare to other categories? (Red dashed lines indicate peak months)",
           x = "Month", y = "Total Spending ($)") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.major = element_line(color = "grey80"),
            panel.grid.minor = element_line(color = "grey90"),
            legend.position = "bottom") +
      scale_color_manual(values = travel_colors[names(travel_colors) %in% trend_data$Category])
    ggplotly(p, tooltip = c("Month", "Total_Spending", "Category"))
  })
  
  # Stacked bar plot for day of week patterns
  output$day_of_week_patterns_plot <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) {
      return(NULL)
    }
    
    day_summary <- data %>%
      mutate(Day_of_Week = weekdays(Date)) %>%
      group_by(Day_of_Week, Category) %>%
      summarise(Total_Spending = sum(`Transaction Amount`), .groups = "drop") %>%
      mutate(Day_of_Week = factor(Day_of_Week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))
    
    p <- ggplot(day_summary, aes(x = Day_of_Week, y = Total_Spending, fill = Category)) +
      geom_bar(stat = "identity") +
      labs(title = "Day of Week Spending Patterns",
           subtitle = "How does spending vary by day of the week?",
           x = "Day of Week", y = "Total Spending ($)") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.major = element_line(color = "grey80"),
            panel.grid.minor = element_line(color = "grey90"),
            legend.position = "bottom") +
      scale_fill_manual(values = travel_colors[names(travel_colors) %in% day_summary$Category])
    ggplotly(p, tooltip = c("Day_of_Week", "Total_Spending", "Category"))
  })
  
  # Interactive table of aggregated customer segments using DT
  output$customer_segments <- renderDT({
    total_spending <- filtered_data()
    if (nrow(total_spending) == 0) {
      return(datatable(data.frame(Message = "No transactions available for this selection."), options = list(pageLength = 5)))
    }
    
    total_spending <- total_spending %>%
      group_by(`Customer ID`) %>%
      summarise(Total_Spending = sum(`Transaction Amount`), .groups = "drop")
    
    total_spending <- total_spending %>%
      mutate(Spending_Tier = case_when(
        Total_Spending >= quantile(Total_Spending, 0.8) ~ "High Spenders",
        Total_Spending >= quantile(Total_Spending, 0.4) ~ "Medium Spenders",
        TRUE ~ "Low Spenders"
      ))
    
    summary_table <- total_spending %>%
      group_by(Spending_Tier) %>%
      summarise(
        Customer_Count = n(),
        Average_Spending = mean(Total_Spending),
        Total_Spending_Sum = sum(Total_Spending),
        .groups = "drop"
      )
    
    datatable(summary_table, options = list(pageLength = 5, searching = TRUE, ordering = TRUE))
  })
  
  # Interactive table for top 10 spenders
  output$top_spenders <- renderDT({
    top_spenders <- filtered_data()
    if (nrow(top_spenders) == 0) {
      return(datatable(data.frame(Message = "No transactions available for this selection."), options = list(pageLength = 5)))
    }
    
    top_spenders <- top_spenders %>%
      group_by(`Customer ID`, Name, Surname, Gender, Age_Group) %>%
      summarise(
        Total_Spending = sum(`Transaction Amount`),
        Transaction_Count = n(),
        .groups = "drop"
      ) %>%
      arrange(desc(Total_Spending)) %>%
      slice_head(n = 10)
    
    datatable(top_spenders, options = list(pageLength = 5, searching = TRUE, ordering = TRUE))
  })
  
  # Interactive table for top 10 merchants
  output$top_merchants <- renderDT({
    top_merchants <- filtered_data()
    if (nrow(top_merchants) == 0) {
      return(datatable(data.frame(Message = "No transactions available for this selection."), options = list(pageLength = 5)))
    }
    
    top_merchants <- top_merchants %>%
      group_by(`Merchant Name`) %>%
      summarise(
        Total_Spending = sum(`Transaction Amount`),
        Transaction_Count = n(),
        .groups = "drop"
      ) %>%
      arrange(desc(Total_Spending)) %>%
      slice_head(n = 10)
    
    datatable(top_merchants, options = list(pageLength = 5, searching = TRUE, ordering = TRUE))
  })
  
  # Plot for top merchants for high spenders in Travel
  output$top_merchants_high_spenders_travel <- renderPlotly({
    high_spenders_travel <- df %>%
      filter(Category == "Travel") %>%
      group_by(`Customer ID`) %>%
      summarise(Total_Spending = sum(`Transaction Amount`), .groups = "drop") %>%
      mutate(Spending_Tier = case_when(
        Total_Spending >= quantile(Total_Spending, 0.8) ~ "High Spenders",
        Total_Spending >= quantile(Total_Spending, 0.4) ~ "Medium Spenders",
        TRUE ~ "Low Spenders"
      )) %>%
      filter(Spending_Tier == "High Spenders")
    
    if (nrow(high_spenders_travel) == 0) {
      return(NULL)
    }
    
    top_merchants <- df %>%
      filter(`Customer ID` %in% high_spenders_travel$`Customer ID`, Category == "Travel") %>%
      group_by(`Merchant Name`) %>%
      summarise(Total_Spending = sum(`Transaction Amount`), .groups = "drop") %>%
      arrange(desc(Total_Spending)) %>%
      slice_head(n = 10)
    
    if (nrow(top_merchants) == 0) {
      return(NULL)
    }
    
    p <- ggplot(top_merchants, aes(x = reorder(`Merchant Name`, Total_Spending), y = Total_Spending, fill = `Merchant Name`)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(Total_Spending, 2)), hjust = -0.2) +
      coord_flip() +
      labs(title = "Top Merchants for High Spenders in Travel",
           subtitle = "Which merchants are most popular among high spenders in Travel?",
           x = "Merchant Name", y = "Total Spending ($)") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.major = element_line(color = "grey80"),
            panel.grid.minor = element_line(color = "grey90"),
            legend.position = "none") +
      scale_fill_manual(values = travel_colors)
    ggplotly(p, tooltip = c("Merchant Name", "Total_Spending"))
  })
  
  # Download handler for customer segments table
  output$download_segments <- downloadHandler(
    filename = function() {
      paste("customer_segments_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      total_spending <- filtered_data()
      if (nrow(total_spending) == 0) {
        return(NULL)
      }
      
      total_spending <- total_spending %>%
        group_by(`Customer ID`) %>%
        summarise(Total_Spending = sum(`Transaction Amount`), .groups = "drop")
      
      total_spending <- total_spending %>%
        mutate(Spending_Tier = case_when(
          Total_Spending >= quantile(Total_Spending, 0.8) ~ "High Spenders",
          Total_Spending >= quantile(Total_Spending, 0.4) ~ "Medium Spenders",
          TRUE ~ "Low Spenders"
        ))
      
      summary_table <- total_spending %>%
        group_by(Spending_Tier) %>%
        summarise(
          Customer_Count = n(),
          Average_Spending = mean(Total_Spending),
          Total_Spending_Sum = sum(Total_Spending),
          .groups = "drop"
        )
      
      write.csv(summary_table, file, row.names = FALSE)
    }
  )
}
# Run the Shiny app
shinyApp(ui = ui, server = server)



