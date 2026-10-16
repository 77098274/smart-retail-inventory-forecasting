
library(shiny)
library(ggplot2)
library(bslib)

# Load historical retail data
data <- read.csv("retail_sales.csv", stringsAsFactors = FALSE)
data <- subset(data, !is.na(Price))
data$SalesDate <- as.Date(data$SalesDate, format = "%d-%m-%Y")
data$Revenue <- data$Price * data$UnitsSold

# Load machine learning output files
forecast_data <- read.csv(
  "retail_demand_forecast.csv",
  stringsAsFactors = FALSE
)
forecast_data$PredictionDate <- as.Date(forecast_data$PredictionDate)

restock_data <- read.csv(
  "inventory_recommendations.csv",
  stringsAsFactors = FALSE
)

metrics_file <- if (file.exists("final_model_metrics.csv")) {
  "final_model_metrics.csv"
} else {
  "model_metrics.csv"
}
model_metrics <- read.csv(metrics_file, stringsAsFactors = FALSE)

# Product sales summary
product_sales <- aggregate(UnitsSold ~ ProductName, data = data, sum)
product_sales <- product_sales[order(-product_sales$UnitsSold), ]
product_sales$Movement <- ifelse(
  product_sales$UnitsSold >= mean(product_sales$UnitsSold),
  "Fast-Moving",
  "Slow-Moving"
)

# Category revenue
category_revenue <- aggregate(Revenue ~ Category, data = data, sum)

# Latest stock per product
latest_stock <- data[order(data$SalesDate), ]
latest_stock <- subset(
  latest_stock,
  !duplicated(ProductName, fromLast = TRUE)
)
latest_stock$StockStatus <- ifelse(
  latest_stock$CurrentStock < 50,
  "Low Stock",
  "Sufficient Stock"
)

# Dashboard UI
ui <- page_sidebar(
  title = "Smart Retail Analytics",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2457C5",
    secondary = "#64748B",
    bg = "#F5F7FB",
    fg = "#172033"
  ),
  
  sidebar = sidebar(
    title = "Dashboard Filters",
    selectInput(
      "product",
      "Select Product",
      choices = c("All Products", sort(unique(data$ProductName))),
      selected = "All Products"
    ),
    selectInput(
      "category",
      "Select Category",
      choices = c("All Categories", sort(unique(data$Category))),
      selected = "All Categories"
    ),
    hr(),
    p(
      "Explore sales, revenue, product movement, stock levels and AI-based demand forecasts.",
      class = "text-muted"
    ),
    width = 280
  ),
  
  tags$head(
    tags$style(HTML("
      body { background-color: #F5F7FB; }
      .navbar { box-shadow: 0 2px 10px rgba(0,0,0,0.06); }
      .dashboard-subtitle { color: #64748B; margin-bottom: 24px; }
      .metric-card {
        background: white; border: 1px solid #E4EAF3;
        border-radius: 14px; padding: 20px; min-height: 125px;
        box-shadow: 0 4px 14px rgba(30,50,90,0.04);
        margin-bottom: 16px;
      }
      .metric-label {
        color: #64748B; font-size: 13px;
        font-weight: 600; margin-bottom: 12px;
      }
      .metric-value {
        color: #172033; font-size: 26px;
        font-weight: 750; overflow-wrap: anywhere;
      }
      .section-card {
        background: white; border: 1px solid #E4EAF3;
        border-radius: 14px; padding: 22px; margin-bottom: 22px;
        box-shadow: 0 4px 14px rgba(30,50,90,0.04);
      }
      .section-title {
        font-size: 18px; font-weight: 700;
        margin-top: 0; margin-bottom: 18px; color: #172033;
      }
      .sidebar, .table { background: white; }
      .control-label { font-weight: 600; }
    "))
  ),
  
  div(
    class = "container-fluid",
    h2("Retail Performance Overview"),
    p(
      class = "dashboard-subtitle",
      "Monitor sales performance, product demand and inventory health."
    ),
    
    uiOutput("metric_cards"),
    
    fluidRow(
      column(
        6,
        div(
          class = "section-card",
          h3(class = "section-title", "Revenue by Category"),
          plotOutput("revenue_plot", height = "300px")
        )
      ),
      column(
        6,
        div(
          class = "section-card",
          h3(class = "section-title", "Top 6 Best-Selling Products"),
          plotOutput("top_products_plot", height = "300px")
        )
      )
    ),
    
    fluidRow(
      column(
        12,
        div(
          class = "section-card",
          h3(class = "section-title", "Daily Sales Trend"),
          plotOutput("daily_sales_plot", height = "320px")
        )
      )
    ),
    
    # New machine learning section
    h2("AI Demand Forecasting"),
    p(
      class = "dashboard-subtitle",
      paste(
        "Random Forest predictions for",
        format(min(forecast_data$PredictionDate), "%d %b %Y"),
        "to",
        format(max(forecast_data$PredictionDate), "%d %b %Y")
      )
    ),
    
    uiOutput("ml_metric_cards"),
    
    fluidRow(
      column(
        7,
        div(
          class = "section-card",
          h3(class = "section-title", "7-Day Predicted Demand"),
          plotOutput("forecast_plot", height = "340px")
        )
      ),
      column(
        5,
        div(
          class = "section-card",
          h3(class = "section-title", "Forecast Details"),
          tableOutput("forecast_table")
        )
      )
    ),
    
    fluidRow(
      column(
        12,
        div(
          class = "section-card",
          h3(class = "section-title", "Smart Restocking Recommendations"),
          p(
            class = "text-muted",
            "Recommended quantities are based on predicted seven-day demand and current stock."
          ),
          tableOutput("restock_table")
        )
      )
    ),
    
    fluidRow(
      column(
        6,
        div(
          class = "section-card",
          h3(class = "section-title", "Product Movement"),
          tableOutput("movement_table")
        )
      ),
      column(
        6,
        div(
          class = "section-card",
          h3(class = "section-title", "Low-Stock Alerts"),
          tableOutput("stock_table")
        )
      )
    ),
    
    p(
      class = "text-muted text-center",
      "Smart Retail Inventory Management System | Powered by R Shiny and Random Forest"
    )
  )
)

# Dashboard server
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    result <- data
    
    if (input$product != "All Products") {
      result <- subset(result, ProductName == input$product)
    }
    
    if (input$category != "All Categories") {
      result <- subset(result, Category == input$category)
    }
    
    result
  })
  
  selected_products <- reactive({
    unique(filtered_data()$ProductName)
  })
  
  filtered_forecast <- reactive({
    result <- forecast_data[
      forecast_data$ProductName %in% selected_products(),
      ,
      drop = FALSE
    ]
    
    if (input$product != "All Products") {
      result <- result[result$ProductName == input$product, , drop = FALSE]
    }
    
    result
  })
  
  filtered_restock <- reactive({
    result <- restock_data[
      restock_data$ProductName %in% selected_products(),
      ,
      drop = FALSE
    ]
    
    if (input$product != "All Products") {
      result <- result[result$ProductName == input$product, , drop = FALSE]
    }
    
    result
  })
  
  output$metric_cards <- renderUI({
    current_data <- filtered_data()
    
    total_revenue <- sum(current_data$Revenue, na.rm = TRUE)
    total_units <- sum(current_data$UnitsSold, na.rm = TRUE)
    product_count <- length(unique(current_data$ProductName))
    
    low_stock_count <- sum(
      latest_stock$ProductName %in% unique(current_data$ProductName) &
        latest_stock$StockStatus == "Low Stock"
    )
    
    fluidRow(
      column(
        3,
        div(
          class = "metric-card",
          div(class = "metric-label", "TOTAL REVENUE"),
          div(
            class = "metric-value",
            paste0("₹", format(round(total_revenue),
                               big.mark = ",", scientific = FALSE
            ))
          )
        )
      ),
      column(
        3,
        div(
          class = "metric-card",
          div(class = "metric-label", "UNITS SOLD"),
          div(class = "metric-value", format(total_units, big.mark = ","))
        )
      ),
      column(
        3,
        div(
          class = "metric-card",
          div(class = "metric-label", "PRODUCTS"),
          div(class = "metric-value", product_count)
        )
      ),
      column(
        3,
        div(
          class = "metric-card",
          div(class = "metric-label", "LOW-STOCK PRODUCTS"),
          div(class = "metric-value", low_stock_count)
        )
      )
    )
  })
  
  output$ml_metric_cards <- renderUI({
    get_metric <- function(metric_name) {
      values <- model_metrics$Value[
        tolower(model_metrics$Metric) == tolower(metric_name)
      ]
      
      if (length(values) == 0 || is.na(values[1])) {
        return("N/A")
      }
      
      round(as.numeric(values[1]), 3)
    }
    
    fluidRow(
      column(
        4,
        div(
          class = "metric-card",
          div(class = "metric-label", "MEAN ABSOLUTE ERROR"),
          div(class = "metric-value", get_metric("MAE"))
        )
      ),
      column(
        4,
        div(
          class = "metric-card",
          div(class = "metric-label", "ROOT MEAN SQUARED ERROR"),
          div(class = "metric-value", get_metric("RMSE"))
        )
      ),
      column(
        4,
        div(
          class = "metric-card",
          div(class = "metric-label", "R-SQUARED"),
          div(
            class = "metric-value",
            if (is.numeric(get_metric("R-squared"))) {
              paste0(round(as.numeric(get_metric("R-squared")) * 100, 1), "%")
            } else {
              get_metric("R-squared")
            }
          )
        )
      )
    )
  })
  
  output$revenue_plot <- renderPlot({
    current_data <- filtered_data()
    validate(need(nrow(current_data) > 0, "No data for these filters."))
    
    result <- aggregate(Revenue ~ Category, data = current_data, sum)
    
    ggplot(result, aes(x = reorder(Category, Revenue), y = Revenue)) +
      geom_col(fill = "#2457C5", width = 0.7) +
      coord_flip() +
      scale_y_continuous(labels = function(x) {
        paste0("₹", format(x, big.mark = ",", scientific = FALSE))
      }) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "#475569")
      )
  })
  
  output$top_products_plot <- renderPlot({
    current_data <- filtered_data()
    validate(need(nrow(current_data) > 0, "No data for these filters."))
    
    result <- aggregate(UnitsSold ~ ProductName, data = current_data, sum)
    result <- head(result[order(-result$UnitsSold), ], 6)
    
    ggplot(result, aes(x = reorder(ProductName, UnitsSold), y = UnitsSold)) +
      geom_col(fill = "#159A83", width = 0.7) +
      coord_flip() +
      labs(x = NULL, y = "Units Sold") +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "#475569")
      )
  })
  
  output$daily_sales_plot <- renderPlot({
    current_data <- filtered_data()
    validate(need(nrow(current_data) > 0, "No data for these filters."))
    
    result <- aggregate(UnitsSold ~ SalesDate, data = current_data, sum)
    result <- result[order(result$SalesDate), ]
    
    ggplot(result, aes(x = SalesDate, y = UnitsSold)) +
      geom_line(color = "#7656C9", linewidth = 0.8) +
      labs(x = "Date", y = "Units Sold") +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "#475569")
      )
  })
  
  output$forecast_plot <- renderPlot({
    result <- filtered_forecast()
    validate(need(nrow(result) > 0, "No forecast available for these filters."))
    
    daily <- aggregate(
      PredictedDemand ~ PredictionDate,
      data = result,
      sum
    )
    
    ggplot(daily, aes(x = PredictionDate, y = PredictedDemand)) +
      geom_line(color = "#2457C5", linewidth = 1) +
      geom_point(color = "#159A83", size = 2.5) +
      labs(x = "Forecast Date", y = "Predicted Units") +
      scale_x_date(date_labels = "%d %b", date_breaks = "1 day") +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(
          color = "#475569", angle = 35, hjust = 1
        )
      )
  })
  
  output$forecast_table <- renderTable({
    result <- filtered_forecast()
    validate(need(nrow(result) > 0, "No forecast available."))
    
    result <- result[, c(
      "ProductName", "PredictionDate", "PredictedDemand"
    )]
    
    result$PredictionDate <- format(result$PredictionDate, "%d %b")
    names(result) <- c("Product", "Date", "Predicted Units")
    result
  }, striped = TRUE, bordered = FALSE, spacing = "s")
  
  output$restock_table <- renderTable({
    result <- filtered_restock()
    validate(need(nrow(result) > 0, "No recommendations available."))
    
    wanted <- intersect(
      c(
        "ProductName", "CurrentStock", "Predicted7DayDemand",
        "RecommendedRestock", "InventoryStatus"
      ),
      names(result)
    )
    
    result <- result[, wanted, drop = FALSE]
    
    display_names <- c(
      ProductName = "Product",
      CurrentStock = "Current Stock",
      Predicted7DayDemand = "7-Day Demand",
      RecommendedRestock = "Recommended Restock",
      InventoryStatus = "Inventory Status"
    )
    
    names(result) <- unname(display_names[wanted])
    result
  }, striped = TRUE, bordered = FALSE, spacing = "s")
  
  output$movement_table <- renderTable({
    current_data <- filtered_data()
    validate(need(nrow(current_data) > 0, "No data for these filters."))
    
    result <- aggregate(UnitsSold ~ ProductName, data = current_data, sum)
    result$Movement <- ifelse(
      result$UnitsSold >= mean(result$UnitsSold),
      "Fast-Moving",
      "Slow-Moving"
    )
    result <- result[order(-result$UnitsSold), ]
    names(result) <- c("Product", "Units Sold", "Movement")
    result
  }, striped = TRUE, bordered = FALSE, spacing = "s")
  
  output$stock_table <- renderTable({
    current_data <- filtered_data()
    
    result <- subset(
      latest_stock,
      ProductName %in% unique(current_data$ProductName) &
        StockStatus == "Low Stock",
      select = c("ProductName", "CurrentStock", "StockStatus")
    )
    
    names(result) <- c("Product", "Current Stock", "Status")
    result
  }, striped = TRUE, bordered = FALSE, spacing = "s")
}

shinyApp(ui = ui, server = server)
