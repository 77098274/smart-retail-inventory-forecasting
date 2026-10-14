
library(shiny)
library(ggplot2)
library(bslib)

# Load and prepare dataset
data <- read.csv("retail_sales.csv", stringsAsFactors = FALSE)

data <- subset(data, !is.na(Price))

data$SalesDate <- as.Date(data$SalesDate, format = "%d-%m-%Y")
data$Revenue <- data$Price * data$UnitsSold

# Product sales summary
product_sales <- aggregate(
  UnitsSold ~ ProductName,
  data = data,
  sum
)

product_sales <- product_sales[
  order(-product_sales$UnitsSold),
]

product_sales$Movement <- ifelse(
  product_sales$UnitsSold >= mean(product_sales$UnitsSold),
  "Fast-Moving",
  "Slow-Moving"
)

# Category revenue
category_revenue <- aggregate(
  Revenue ~ Category,
  data = data,
  sum
)

# Daily sales
daily_sales <- aggregate(
  UnitsSold ~ SalesDate,
  data = data,
  sum
)

daily_sales <- daily_sales[
  order(daily_sales$SalesDate),
]

# Latest stock per product
latest_stock <- data[
  order(data$SalesDate),
]

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
      choices = c(
        "All Products",
        sort(unique(data$ProductName))
      ),
      selected = "All Products"
    ),
    
    selectInput(
      "category",
      "Select Category",
      choices = c(
        "All Categories",
        sort(unique(data$Category))
      ),
      selected = "All Categories"
    ),
    
    hr(),
    
    p(
      "Explore sales, revenue, product movement and stock levels.",
      class = "text-muted"
    ),
    
    width = 280
  ),
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #F5F7FB;
      }

      .navbar {
        box-shadow: 0 2px 10px rgba(0,0,0,0.06);
      }

      .dashboard-subtitle {
        color: #64748B;
        margin-bottom: 24px;
      }

      .metric-card {
        background: white;
        border: 1px solid #E4EAF3;
        border-radius: 14px;
        padding: 20px;
        min-height: 125px;
        box-shadow: 0 4px 14px rgba(30,50,90,0.04);
        margin-bottom: 16px;
      }

      .metric-label {
        color: #64748B;
        font-size: 13px;
        font-weight: 600;
        margin-bottom: 12px;
      }

      .metric-value {
        color: #172033;
        font-size: 26px;
        font-weight: 750;
        overflow-wrap: anywhere;
      }

      .section-card {
        background: white;
        border: 1px solid #E4EAF3;
        border-radius: 14px;
        padding: 22px;
        margin-bottom: 22px;
        box-shadow: 0 4px 14px rgba(30,50,90,0.04);
      }

      .section-title {
        font-size: 18px;
        font-weight: 700;
        margin-top: 0;
        margin-bottom: 18px;
        color: #172033;
      }

      .sidebar {
        background: white;
      }

      .table {
        background: white;
      }

      .control-label {
        font-weight: 600;
      }
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
          h3(
            class = "section-title",
            "Revenue by Category"
          ),
          plotOutput("revenue_plot", height = "300px")
        )
      ),
      
      column(
        6,
        div(
          class = "section-card",
          h3(
            class = "section-title",
            "Top 6 Best-Selling Products"
          ),
          plotOutput("top_products_plot", height = "300px")
        )
      )
    ),
    
    fluidRow(
      column(
        12,
        div(
          class = "section-card",
          h3(
            class = "section-title",
            "Daily Sales Trend"
          ),
          plotOutput("daily_sales_plot", height = "320px")
        )
      )
    ),
    
    fluidRow(
      column(
        6,
        div(
          class = "section-card",
          h3(
            class = "section-title",
            "Product Movement"
          ),
          tableOutput("movement_table")
        )
      ),
      
      column(
        6,
        div(
          class = "section-card",
          h3(
            class = "section-title",
            "Low-Stock Alerts"
          ),
          tableOutput("stock_table")
        )
      )
    ),
    
    p(
      class = "text-muted text-center",
      "Smart Retail Inventory Management System | Powered by R Shiny"
    )
  )
)

# Dashboard server
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    result <- data
    
    if (input$product != "All Products") {
      result <- subset(
        result,
        ProductName == input$product
      )
    }
    
    if (input$category != "All Categories") {
      result <- subset(
        result,
        Category == input$category
      )
    }
    
    result
  })
  
  output$metric_cards <- renderUI({
    current_data <- filtered_data()
    
    total_revenue <- sum(
      current_data$Revenue,
      na.rm = TRUE
    )
    
    total_units <- sum(
      current_data$UnitsSold,
      na.rm = TRUE
    )
    
    product_count <- length(
      unique(current_data$ProductName)
    )
    
    low_stock_count <- sum(
      latest_stock$ProductName %in%
        unique(current_data$ProductName) &
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
            paste0("₹", format(
              round(total_revenue),
              big.mark = ",",
              scientific = FALSE
            ))
          )
        )
      ),
      
      column(
        3,
        div(
          class = "metric-card",
          div(class = "metric-label", "UNITS SOLD"),
          div(
            class = "metric-value",
            format(total_units, big.mark = ",")
          )
        )
      ),
      
      column(
        3,
        div(
          class = "metric-card",
          div(class = "metric-label", "PRODUCTS"),
          div(
            class = "metric-value",
            product_count
          )
        )
      ),
      
      column(
        3,
        div(
          class = "metric-card",
          div(class = "metric-label", "LOW-STOCK PRODUCTS"),
          div(
            class = "metric-value",
            low_stock_count
          )
        )
      )
    )
  })
  
  output$revenue_plot <- renderPlot({
    current_data <- filtered_data()
    
    validate(
      need(nrow(current_data) > 0, "No data for these filters.")
    )
    
    result <- aggregate(
      Revenue ~ Category,
      data = current_data,
      sum
    )
    
    ggplot(
      result,
      aes(
        x = reorder(Category, Revenue),
        y = Revenue
      )
    ) +
      geom_col(
        fill = "#2457C5",
        width = 0.7
      ) +
      coord_flip() +
      scale_y_continuous(
        labels = function(x) {
          paste0("₹", format(
            x,
            big.mark = ",",
            scientific = FALSE
          ))
        }
      ) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "#475569")
      )
  })
  
  output$top_products_plot <- renderPlot({
    current_data <- filtered_data()
    
    validate(
      need(nrow(current_data) > 0, "No data for these filters.")
    )
    
    result <- aggregate(
      UnitsSold ~ ProductName,
      data = current_data,
      sum
    )
    
    result <- head(
      result[order(-result$UnitsSold), ],
      6
    )
    
    ggplot(
      result,
      aes(
        x = reorder(ProductName, UnitsSold),
        y = UnitsSold
      )
    ) +
      geom_col(
        fill = "#159A83",
        width = 0.7
      ) +
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
    
    validate(
      need(nrow(current_data) > 0, "No data for these filters.")
    )
    
    result <- aggregate(
      UnitsSold ~ SalesDate,
      data = current_data,
      sum
    )
    
    result <- result[order(result$SalesDate), ]
    
    ggplot(
      result,
      aes(x = SalesDate, y = UnitsSold)
    ) +
      geom_line(
        color = "#7656C9",
        linewidth = 0.8
      ) +
      labs(x = "Date", y = "Units Sold") +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "#475569")
      )
  })
  
  output$movement_table <- renderTable({
    current_data <- filtered_data()
    
    validate(
      need(nrow(current_data) > 0, "No data for these filters.")
    )
    
    result <- aggregate(
      UnitsSold ~ ProductName,
      data = current_data,
      sum
    )
    
    result$Movement <- ifelse(
      result$UnitsSold >= mean(result$UnitsSold),
      "Fast-Moving",
      "Slow-Moving"
    )
    
    result <- result[order(-result$UnitsSold), ]
    
    names(result) <- c(
      "Product",
      "Units Sold",
      "Movement"
    )
    
    result
  }, striped = TRUE, bordered = FALSE, spacing = "s")
  
  output$stock_table <- renderTable({
    current_data <- filtered_data()
    
    result <- subset(
      latest_stock,
      ProductName %in% unique(current_data$ProductName) &
        StockStatus == "Low Stock",
      select = c(
        ProductName,
        CurrentStock,
        StockStatus
      )
    )
    
    names(result) <- c(
      "Product",
      "Current Stock",
      "Status"
    )
    
    result
  }, striped = TRUE, bordered = FALSE, spacing = "s")
}

shinyApp(ui = ui, server = server)

