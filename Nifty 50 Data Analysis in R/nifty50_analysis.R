# Load required libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)

# Read the data
nifty_data <- read.csv("NSE-NIFTY50.csv")

data <- read.csv("NSE-NIFTY50.csv")

# Data cleaning steps
nifty_data_clean <- nifty_data %>%
  # Remove any rows with NA values
  na.omit() %>%
  # Ensure numeric columns are properly formatted
  mutate(
    across(c(Open, High, Low, Close, VWAP, Volume), as.numeric)
  )

# Calculate basic statistics
summary_stats <- summary(nifty_data_clean[c("Open", "High", "Low", "Close")])

# Create a function to detect outliers using IQR method

detect_outliers <- function(x) {
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  x < lower_bound | x > upper_bound
}

# Check for outliers in price data
price_outliers <- detect_outliers(nifty_data_clean$Close)


# 1. Price Movement Over Time
ggplot(nifty_data_clean, aes(x = Date, y = Close)) +
  geom_line(color = "#2c3e50") +
  geom_smooth(method = "loess", color = "#e74c3c", se = FALSE) +
  theme_minimal() +
  labs(
    title = "NIFTY 50 Stock Price Movement",
    subtitle = "Daily closing prices with trend line",
    x = "Date",
    y = "Closing Price",
    caption = "Data source: NSE"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )



# 2. Volume Analysis
ggplot(nifty_data_clean, aes(x = Date, y = Volume)) +
  geom_bar(stat = "identity", fill = "#3498db") +
  theme_minimal() +
  labs(
    title = "Trading Volume Over Time",
    x = "Date",
    y = "Volume",
    caption = "Data source: NSE"
  ) +
  scale_y_continuous(labels = scales::comma)


# 3. Price Range (Candlestick-like visualization)
ggplot(nifty_data_clean, aes(x = Date)) +
  geom_linerange(aes(ymin = Low, ymax = High), color = "#2ecc71", size = 0.5) +
  geom_point(aes(y = Close), color = "#e74c3c", size = 0.5) +
  theme_minimal() +
  labs(
    title = "Daily Price Range",
    subtitle = "High-Low range with closing price",
    x = "Date",
    y = "Price",
    caption = "Data source: NSE"
  )



# 4. Volume-Price Relationship
ggplot(nifty_data_clean, aes(x = Volume, y = Close)) +
  geom_point(alpha = 0.5, color = "#16a085") +
  geom_smooth(method = "lm", color = "#c0392b") +
  theme_minimal() +
  labs(
    title = "Volume vs Price Relationship",
    x = "Trading Volume",
    y = "Closing Price",
    caption = "Data source: NSE"
  ) +
  scale_x_continuous(labels = scales::comma)
