library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(leaflet)
library(lubridate)
library(tidygeocoder)
library(DT)

# ---- LOAD DATA ----
source("R/load_data.R")
data <- load_and_prepare_data()

# ---- GEOCODING ----
locations <- data %>%
  select(city, country) %>%
  distinct() %>%
  geocode(city = city, country = country, method = "osm",
          lat = latitude, long = longitude)

data <- left_join(data, locations, by = c("city", "country"))

data <- data %>%
  mutate(year_month = floor_date(order_date, "month"))

# ---- UI ----
ui <- dashboardPage(
  dashboardHeader(title = "🍫 Chocolate Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview"),
      menuItem("Products", tabName = "products"),
      menuItem("Customers", tabName = "customers"),
      menuItem("Map", tabName = "map")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("overview",
              fluidRow(
                valueBoxOutput("revenue_box"),
                valueBoxOutput("profit_box"),
                valueBoxOutput("margin_box")
              ),
              fluidRow(
                box(plotOutput("time_plot"), width = 12)
              )
      ),
      
      tabItem("products",
              fluidRow(
                box(plotOutput("top_products"), width = 6),
                box(plotOutput("product_profit"), width = 6)
              )
      ),
      
      tabItem("customers",
              fluidRow(
                box(plotOutput("customer_dist"), width = 6),
                box(plotOutput("top_customers"), width = 6)
              )
      ),
      
      tabItem("map",
              fluidRow(
                column(5,
                       box(leafletOutput("sales_map", height = 450), width = NULL),
                       box(selectInput("month_select", "Select Month",
                                       choices = sort(unique(format(data$order_date, "%Y-%m"))),
                                       selected = format(min(data$order_date), "%Y-%m")),
                           width = NULL)
                ),
                column(7,
                       box(plotOutput("product_dist", height = 250), width = NULL),
                       box(DTOutput("product_table"), width = NULL)
                )
              )
      )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  # KPIs
  output$revenue_box <- renderValueBox({
    valueBox(round(sum(data$revenue)), "Total Revenue", color = "red")
  })
  
  output$profit_box <- renderValueBox({
    valueBox(round(sum(data$profit)), "Total Profit", color = "green")
  })
  
  output$margin_box <- renderValueBox({
    valueBox(paste0(round(mean(data$margin)*100, 1), "%"), "Average Margin", color = "olive")
  })
  
  # Time series
  output$time_plot <- renderPlot({
    data %>%
      group_by(order_date) %>%
      summarise(revenue = sum(revenue), profit = sum(profit)) %>%
      ggplot(aes(order_date)) +
      geom_line(aes(y = revenue, color = "Revenue")) +
      geom_line(aes(y = profit, color = "Profit")) +
      scale_color_manual(values = c("Revenue" = "red", "Profit" = "darkgreen")) +
      labs(title = "Revenue vs Profit Over Time")
  })
  
  # Products
  output$top_products <- renderPlot({
    data %>%
      group_by(product_name) %>%
      summarise(total = sum(revenue)) %>%
      slice_max(total, n = 10) %>%
      ggplot(aes(reorder(product_name, total), total)) +
      geom_col(fill = "red") +
      coord_flip() +
      labs(title = "Top Products by Revenue")
  })
  
  output$product_profit <- renderPlot({
    data %>%
      group_by(product_name) %>%
      summarise(total = sum(profit)) %>%
      slice_max(total, n = 10) %>%
      ggplot(aes(reorder(product_name, total), total)) +
      geom_col(fill = "darkgreen") +
      coord_flip() +
      labs(title = "Top Products by Profit")
  })
  
  # Customers
  output$customer_dist <- renderPlot({
    data %>%
      group_by(customer_id) %>%
      summarise(total = sum(revenue)) %>%
      ggplot(aes(total)) +
      geom_histogram(bins = 30, fill = "darkred") +
      labs(title = "Customer Spending Distribution")
  })
  
  output$top_customers <- renderPlot({
    data %>%
      group_by(customer_id) %>%
      summarise(total = sum(revenue)) %>%
      slice_max(total, n = 10) %>%
      ggplot(aes(reorder(as.factor(customer_id), total), total)) +
      geom_col(fill = "purple") +
      coord_flip() +
      labs(title = "Top Customers by Revenue")
  })
  
  # Map
  filtered_data_month <- reactive({
    req(input$month_select)
    selected_month <- as.Date(paste0(input$month_select, "-01"))
    data %>% filter(floor_date(order_date, "month") == selected_month)
  })
  
  map_data <- reactive({
    filtered_data_month() %>%
      group_by(city, country, latitude, longitude) %>%
      summarise(
        revenue = sum(revenue),
        profit = sum(profit),
        sales = n()
      ) %>%
      filter(!is.na(latitude))
  })
  
  output$sales_map <- renderLeaflet({
    df <- map_data()
    leaflet(df) %>%
      addTiles() %>%
      addCircleMarkers(
        ~longitude, ~latitude,
        radius = ~sqrt(revenue)/10 + 3,
        color = "red",
        popup = ~paste0(city, ", ", country, "<br>",
                        "Revenue: ", round(revenue), " €<br>",
                        "Profit: ", round(profit), " €<br>",
                        "Sales: ", sales)
      )
  })
  
  selected_city <- reactiveVal(NULL)
  
  observeEvent(input$sales_map_marker_click, {
    selected_city(input$sales_map_marker_click$id)
  })
  
  output$product_dist <- renderPlot({
    req(selected_city())
    filtered_data_month() %>%
      filter(city == selected_city()) %>%
      group_by(product_name) %>%
      summarise(revenue = sum(revenue)) %>%
      ggplot(aes(reorder(product_name, revenue), revenue, fill = product_name)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(title = paste("Revenue by Product in", selected_city()))
  })
  
  output$product_table <- renderDT({
    req(selected_city())
    filtered_data_month() %>%
      filter(city == selected_city()) %>%
      group_by(product_name) %>%
      summarise(
        Revenue = sum(revenue),
        Profit = sum(profit)
      ) %>%
      arrange(desc(Revenue))
  })
}

shinyApp(ui, server)
