
# Reload the original file without changing dates
raw_check <- read.csv(
  "C:/Users/vishnu miruthulaa/Downloads/retail_sales.csv",
  stringsAsFactors = FALSE,
  colClasses = "character"
)

# Inspect the original date values
print(head(raw_check$SalesDate, 10))

# Check the column name
print(names(raw_check))


data <- raw_check

data$SalesDate <- as.Date(
  data$SalesDate,
  format = "%Y-%m-%d"
)

print(head(data$SalesDate))
print(range(data$SalesDate))

stopifnot(
  min(data$SalesDate, na.rm = TRUE) == as.Date("2025-01-01"),
  max(data$SalesDate, na.rm = TRUE) == as.Date("2025-12-31")
)



install.packages("zoo")
install.packages("lubridate")  # Run only if not installed
library(lubridate)
library(dplyr)
library(zoo)



# ==========================================
# STEP 2: DATA PREPARATION AND FEATURE ENGINEERING
# ==========================================

library(dplyr)
library(lubridate)
library(zoo)

# Convert columns to correct types
data$SalesDate <- as.Date(data$SalesDate)
data$UnitsSold <- as.numeric(data$UnitsSold)
data$Price <- as.numeric(data$Price)

# Remove invalid essential records
data <- data %>%
  filter(
    !is.na(SalesDate),
    !is.na(ProductID),
    !is.na(ProductName),
    !is.na(UnitsSold),
    UnitsSold >= 0
  )

# Fill missing prices using the product's average price
data <- data %>%
  group_by(ProductID) %>%
  mutate(
    Price = if (all(is.na(Price))) {
      NA_real_
    } else {
      ifelse(
        is.na(Price),
        mean(Price, na.rm = TRUE),
        Price
      )
    }
  ) %>%
  ungroup()

# Aggregate sales per product per day
daily_sales <- data %>%
  group_by(ProductID, ProductName, SalesDate) %>%
  summarise(
    Category = first(Category),
    Price = mean(Price, na.rm = TRUE),
    UnitsSold = sum(UnitsSold),
    .groups = "drop"
  ) %>%
  arrange(ProductID, SalesDate)

# Create historical demand features FIRST
daily_sales <- daily_sales %>%
  group_by(ProductID) %>%
  arrange(SalesDate, .by_group = TRUE) %>%
  mutate(
    Lag1 = lag(UnitsSold, 1),
    Lag7 = lag(UnitsSold, 7),
    RollingMean7 = lag(
      zoo::rollmean(
        UnitsSold,
        k = 7,
        fill = NA,
        align = "right"
      ),
      1
    ),
    DayOfWeek = factor(
      lubridate::wday(SalesDate, week_start = 1),
      levels = 1:7
    ),
    Month = factor(
      lubridate::month(SalesDate),
      levels = 1:12
    )
  ) %>%
  ungroup()

# NOW filter rows after features exist
model_data <- daily_sales %>%
  filter(
    !is.na(Lag1),
    !is.na(Lag7),
    !is.na(RollingMean7),
    !is.na(Price)
  )

# Prepare categorical feature
model_data$Category <- factor(model_data$Category)

# Verify the result
cat("Date range:\n")
print(range(daily_sales$SalesDate))

cat("Total daily records:", nrow(daily_sales), "\n")
cat("Records available for ML:", nrow(model_data), "\n")
cat("Unique products:", length(unique(model_data$ProductID)), "\n")

# Check that all required features exist
print(names(model_data))

# Save the prepared ML dataset
write.csv(
  model_data,
  "C:/Users/vishnu miruthulaa/Downloads/retail_ml_features.csv",
  row.names = FALSE
)

cat("Step 2 completed successfully!\n")


# ==========================================
# STEP 3A: TRAIN-TEST SPLIT
# ==========================================

library(dplyr)

# Sort by date
model_data <- model_data %>%
  arrange(SalesDate)

# Get unique dates
all_dates <- sort(unique(model_data$SalesDate))

# Calculate 80% cutoff
split_index <- floor(length(all_dates) * 0.8)

cutoff_date <- all_dates[split_index]

# Create training data
train_data <- model_data %>%
  filter(SalesDate <= cutoff_date)

# Create testing data
test_data <- model_data %>%
  filter(SalesDate > cutoff_date)

# Display results
cat("Training records:", nrow(train_data), "\n")
cat("Testing records:", nrow(test_data), "\n")
cat("Training ends:", as.character(max(train_data$SalesDate)), "\n")
cat("Testing starts:", as.character(min(test_data$SalesDate)), "\n")

# Verify chronological separation
stopifnot(
  max(train_data$SalesDate) < min(test_data$SalesDate)
)

cat("Training and testing split completed!\n")


# ==========================================
# STEP 3B: RANDOM FOREST REGRESSION
# ==========================================

library(randomForest)

# Set seed for reproducibility
set.seed(123)

# Train the model
rf_model <- randomForest(
  UnitsSold ~ Category +
    Price +
    DayOfWeek +
    Month +
    Lag1 +
    Lag7 +
    RollingMean7,
  data = train_data,
  ntree = 500,
  importance = TRUE
)

# Display model summary
print(rf_model)

# Display feature importance
print(importance(rf_model))

# Plot feature importance
varImpPlot(
  rf_model,
  main = "Random Forest Feature Importance"
)

# Save the trained model
saveRDS(
  rf_model,
  "C:/Users/vishnu miruthulaa/Downloads/random_forest_demand_model.rds"
)

cat("Random Forest model trained and saved successfully!\n")



# STEP 4: MODEL EVALUATION

# Predict demand for the testing data
predicted <- predict(rf_model, newdata = test_data)

# Actual demand
actual <- test_data$UnitsSold

# Calculate evaluation metrics
MAE <- mean(abs(actual - predicted))

RMSE <- sqrt(mean((actual - predicted)^2))

SSE <- sum((actual - predicted)^2)
SST <- sum((actual - mean(actual))^2)

R2 <- if (SST > 0) 1 - (SSE / SST) else NA_real_

# Display results
metrics <- data.frame(
  Metric = c("MAE", "RMSE", "R-squared"),
  Value = c(MAE, RMSE, R2)
)

print(metrics)

# Save evaluation metrics
write.csv(
  metrics,
  "C:/Users/vishnu miruthulaa/Downloads/model_metrics.csv",
  row.names = FALSE
)

# Save actual vs predicted demand
evaluation_results <- data.frame(
  ProductID = test_data$ProductID,
  ProductName = test_data$ProductName,
  SalesDate = test_data$SalesDate,
  ActualDemand = actual,
  PredictedDemand = round(predicted, 2)
)

write.csv(
  evaluation_results,
  "C:/Users/vishnu miruthulaa/Downloads/model_evaluation_predictions.csv",
  row.names = FALSE
)

cat("Model evaluation completed successfully!\n")
cat("Evaluation files saved in your Downloads folder.\n")






# ==========================================
# STEP 5: 7-DAY DEMAND FORECASTING
# ==========================================

library(dplyr)
library(lubridate)
library(randomForest)

# 1. Check that the model and required data exist
if (!exists("rf_model")) {
  stop("Run Step 3B first to train the Random Forest model.")
}

if (!exists("train_data")) {
  stop("Run Step 3A first to create training and testing data.")
}

if (!exists("daily_sales")) {
  stop("Run Step 2 first to create daily_sales.")
}

# 2. Prepare the historical data
daily_sales$SalesDate <- as.Date(daily_sales$SalesDate)

daily_sales <- daily_sales %>%
  arrange(ProductID, SalesDate)

# 3. Identify the last historical date
last_date <- max(daily_sales$SalesDate, na.rm = TRUE)

cat("Last historical date:", as.character(last_date), "\n")

# 4. Generate the next seven dates
future_dates <- seq.Date(
  from = last_date + 1,
  by = "day",
  length.out = 7
)

print(future_dates)

# 5. Initialize the forecast results
future_predictions <- list()

# 6. Get the available products
products <- unique(daily_sales$ProductID)

# 7. Forecast demand for every product
for (product in products) {
  
  history <- daily_sales %>%
    filter(ProductID == product) %>%
    arrange(SalesDate)
  
  if (nrow(history) < 7) {
    next
  }
  
  sales_history <- as.numeric(history$UnitsSold)
  
  product_name <- as.character(history$ProductName[1])
  product_category <- as.character(history$Category[1])
  
  valid_prices <- history$Price[!is.na(history$Price)]
  
  if (length(valid_prices) == 0) {
    stop(paste("No valid price for product:", product_name))
  }
  
  product_price <- as.numeric(tail(valid_prices, 1))
  
  # Predict one day at a time
  for (i in seq_along(future_dates)) {
    
    forecast_date <- future_dates[i]
    
    # Extract date features
    weekday_value <- as.character(
      lubridate::wday(forecast_date, week_start = 1)
    )
    
    month_value <- as.character(
      lubridate::month(forecast_date)
    )
    
    # Prepare model input
    new_data <- data.frame(
      Category = factor(
        product_category,
        levels = levels(train_data$Category)
      ),
      Price = product_price,
      DayOfWeek = factor(
        weekday_value,
        levels = levels(train_data$DayOfWeek)
      ),
      Month = factor(
        month_value,
        levels = levels(train_data$Month)
      ),
      Lag1 = tail(sales_history, 1),
      Lag7 = sales_history[length(sales_history) - 6],
      RollingMean7 = mean(tail(sales_history, 7))
    )
    
    # Check for invalid factor values
    if (anyNA(new_data)) {
      stop(paste(
        "Missing model input for product:", product_name,
        "on", as.character(forecast_date),
        "- check Category, DayOfWeek, Month, Price and lag values."
      ))
    }
    
    # Predict demand
    predicted_demand <- as.numeric(
      predict(rf_model, newdata = new_data)
    )
    
    # Round to non-negative whole units
    predicted_demand <- max(0, round(predicted_demand))
    
    # Store this prediction
    future_predictions[[length(future_predictions) + 1]] <-
      data.frame(
        ProductID = product,
        ProductName = product_name,
        PredictionDate = forecast_date,
        PredictedDemand = predicted_demand
      )
    
    # Add prediction to history for the next day
    sales_history <- c(sales_history, predicted_demand)
  }
  
  cat("Forecast completed for:", product_name, "\n")
}

# 8. Combine the predictions into one data frame
if (length(future_predictions) == 0) {
  stop("No predictions were generated. Check your product data.")
}

forecast_results <- dplyr::bind_rows(future_predictions)

# 9. Display the forecast
print(forecast_results)

cat("\nTotal forecast rows:", nrow(forecast_results), "\n")

# 10. Save the forecast CSV
output_file <- file.path(
  "C:/Users/vishnu miruthulaa/Downloads",
  "retail_demand_forecast.csv"
)

write.csv(
  forecast_results,
  output_file,
  row.names = FALSE
)

cat("\nSTEP 5 COMPLETED SUCCESSFULLY!\n")
cat("File saved at:", output_file, "\n")




# ==========================================
# STEP 6: INVENTORY ANALYSIS AND RESTOCKING
# ==========================================

library(dplyr)

# 1. Check required datasets
if (!exists("forecast_results")) {
  stop("Run Step 5 successfully first.")
}

if (!exists("data")) {
  stop("Run Step 2 data loading and cleaning first.")
}

# 2. Get the latest stock recorded for each product
latest_stock <- data %>%
  mutate(
    SalesDate = as.Date(SalesDate),
    CurrentStock = as.numeric(CurrentStock)
  ) %>%
  filter(!is.na(SalesDate), !is.na(CurrentStock)) %>%
  arrange(SalesDate) %>%
  group_by(ProductID, ProductName) %>%
  summarise(
    CurrentStock = dplyr::last(CurrentStock),
    .groups = "drop"
  )

# 3. Calculate total predicted demand for seven days
weekly_demand <- forecast_results %>%
  group_by(ProductID, ProductName) %>%
  summarise(
    Predicted7DayDemand = sum(PredictedDemand),
    .groups = "drop"
  )

# 4. Combine stock and forecast information
inventory_analysis <- latest_stock %>%
  left_join(
    weekly_demand,
    by = c("ProductID", "ProductName")
  ) %>%
  mutate(
    Predicted7DayDemand = coalesce(Predicted7DayDemand, 0),
    EstimatedStockAfter7Days =
      CurrentStock - Predicted7DayDemand,
    RecommendedRestock = pmax(
      0,
      Predicted7DayDemand - CurrentStock
    ),
    InventoryStatus = case_when(
      CurrentStock <= 0 ~ "Out of Stock",
      CurrentStock < Predicted7DayDemand ~ "Restock Required",
      CurrentStock == Predicted7DayDemand ~ "Stock May Run Out",
      TRUE ~ "Stock Sufficient"
    )
  )

# 5. Display inventory recommendations
print(inventory_analysis)

# 6. Save the results to Downloads
write.csv(
  inventory_analysis,
  "C:/Users/vishnu miruthulaa/Downloads/inventory_recommendations.csv",
  row.names = FALSE
)

cat("\nSTEP 6 COMPLETED SUCCESSFULLY!\n")
cat("Saved: inventory_recommendations.csv\n")



install.packages("ggplot2")
library(ggplot2)
library(dplyr)


# Check forecast results
if (!exists("forecast_results")) {
  stop("Run Step 5 successfully first.")
}

# Demand forecast graph
p1 <- ggplot(
  forecast_results,
  aes(
    x = PredictionDate,
    y = PredictedDemand,
    color = ProductName,
    group = ProductName
  )
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  labs(
    title = "7-Day Retail Demand Forecast",
    x = "Forecast Date",
    y = "Predicted Units Sold",
    color = "Product"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

print(p1)

ggsave(
  "C:/Users/vishnu miruthulaa/Downloads/demand_forecast_graph.png",
  plot = p1,
  width = 10,
  height = 6
)

# Check evaluation data
if (!exists("evaluation_results")) {
  stop("Run Step 4 successfully first.")
}

p2 <- ggplot(
  evaluation_results,
  aes(x = ActualDemand, y = PredictedDemand)
) +
  geom_point(alpha = 0.6) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed"
  ) +
  labs(
    title = "Actual vs Predicted Retail Demand",
    x = "Actual Units Sold",
    y = "Predicted Units Sold"
  ) +
  theme_minimal()

print(p2)

ggsave(
  "C:/Users/vishnu miruthulaa/Downloads/actual_vs_predicted_graph.png",
  plot = p2,
  width = 8,
  height = 6
)


# Check inventory analysis
if (!exists("inventory_analysis")) {
  stop("Run Step 6 successfully first.")
}

p3 <- ggplot(
  inventory_analysis,
  aes(
    x = reorder(ProductName, RecommendedRestock),
    y = RecommendedRestock
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Recommended Restocking Quantity by Product",
    x = "Product",
    y = "Additional Units Recommended"
  ) +
  theme_minimal()

print(p3)

ggsave(
  "C:/Users/vishnu miruthulaa/Downloads/inventory_restock_graph.png",
  plot = p3,
  width = 9,
  height = 6
)



# ==========================================
# STEP 8: FINAL PROJECT SUMMARY
# ==========================================

library(dplyr)

# Check required results
if (!exists("metrics")) {
  stop("Run Step 4 to calculate model metrics.")
}

if (!exists("forecast_results")) {
  stop("Run Step 5 to generate forecasts.")
}

if (!exists("inventory_analysis")) {
  stop("Run Step 6 to generate inventory recommendations.")
}

# 1. Display model evaluation metrics
cat("\n===== MODEL EVALUATION =====\n")
print(metrics)

# 2. Summarize seven-day demand
forecast_summary <- forecast_results %>%
  group_by(ProductID, ProductName) %>%
  summarise(
    TotalPredictedDemand = sum(PredictedDemand),
    AverageDailyDemand = round(mean(PredictedDemand), 2),
    .groups = "drop"
  )

cat("\n===== SEVEN-DAY DEMAND SUMMARY =====\n")
print(forecast_summary)

# 3. Summarize inventory status
cat("\n===== INVENTORY STATUS =====\n")

print(
  inventory_analysis %>%
    select(
      ProductID,
      ProductName,
      CurrentStock,
      Predicted7DayDemand,
      RecommendedRestock,
      InventoryStatus
    )
)

# 4. Count products by inventory status
status_summary <- inventory_analysis %>%
  count(InventoryStatus, name = "NumberOfProducts")

cat("\n===== INVENTORY STATUS COUNTS =====\n")
print(status_summary)

# 5. Save the final report tables
output_folder <- "C:/Users/vishnu miruthulaa/Downloads"

write.csv(
  metrics,
  file.path(output_folder, "final_model_metrics.csv"),
  row.names = FALSE
)

write.csv(
  forecast_summary,
  file.path(output_folder, "final_demand_summary.csv"),
  row.names = FALSE
)

write.csv(
  inventory_analysis,
  file.path(output_folder, "final_inventory_recommendations.csv"),
  row.names = FALSE
)

write.csv(
  status_summary,
  file.path(output_folder, "final_inventory_status_summary.csv"),
  row.names = FALSE
)

cat("\nSTEP 8 COMPLETED SUCCESSFULLY!\n")
cat("All final report files saved in Downloads.\n")


install.packages(c("shiny", "ggplot2", "dplyr"))

