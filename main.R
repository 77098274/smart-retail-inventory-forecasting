library(ggplot2)

data <- read.csv("retail_sales.csv")

print(head(data))
str(data)
summary(data)
print(colSums(is.na(data)))

data <- data[!is.na(data$Price), ]

print(colSums(is.na(data)))
cat("Number of rows after cleaning:", nrow(data), "\n")

data$SalesDate <- as.Date(data$SalesDate, format = "%d-%m-%Y")
data$Revenue <- data$Price * data$UnitsSold

print(head(data[, c("ProductName", "Price", "UnitsSold", "Revenue")]))
cat("Total Revenue: Rs.", sum(data$Revenue), "\n")

product_sales <- aggregate(UnitsSold ~ ProductName, data = data, sum)
product_sales <- product_sales[order(-product_sales$UnitsSold), ]

print(product_sales)

category_revenue <- aggregate(Revenue ~ Category, data = data, sum)
category_revenue <- category_revenue[order(-category_revenue$Revenue), ]

print(category_revenue)

print(
  ggplot(category_revenue, aes(x = reorder(Category, Revenue), y = Revenue)) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Revenue by Product Category",
      x = "Category",
      y = "Revenue (Rs.)"
    ) +
    theme_minimal()
)

print(
  ggplot(head(product_sales, 6),
         aes(x = reorder(ProductName, UnitsSold), y = UnitsSold)) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Top 6 Best-Selling Products",
      x = "Product",
      y = "Total Units Sold"
    ) +
    theme_minimal()
)

daily_sales <- aggregate(UnitsSold ~ SalesDate, data = data, sum)
daily_sales <- daily_sales[order(daily_sales$SalesDate), ]

print(
  ggplot(daily_sales, aes(x = SalesDate, y = UnitsSold)) +
    geom_line() +
    labs(
      title = "Daily Sales Trend",
      x = "Date",
      y = "Total Units Sold"
    ) +
    theme_minimal()
)

product_sales$Movement <- ifelse(
  product_sales$UnitsSold >= mean(product_sales$UnitsSold),
  "Fast-Moving",
  "Slow-Moving"
)

cat("\nProduct Movement Classification:\n")
print(product_sales)

latest_stock <- data[order(data$SalesDate), ]
latest_stock <- latest_stock[!duplicated(latest_stock$ProductName, fromLast = TRUE), ]

latest_stock$StockStatus <- ifelse(
  latest_stock$CurrentStock < 50,
  "Low Stock",
  "Sufficient Stock"
)

stock_alerts <- latest_stock[
  latest_stock$StockStatus == "Low Stock",
  c("ProductName", "CurrentStock", "StockStatus")
]

cat("\nLow-Stock Alerts:\n")
print(stock_alerts)

cat("\nAnalysis Completed Successfully!\n")

