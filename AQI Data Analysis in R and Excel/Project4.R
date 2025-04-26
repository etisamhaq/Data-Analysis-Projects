# Install required packages
install.packages(c("tidyverse", "forecast", "tseries", "ggplot2", "lubridate"))

# Load libraries
library(tidyverse)
library(forecast)
library(tseries)
library(ggplot2)
library(lubridate)
library(corrplot)

# Import data
air_data <- read.csv("air_quality_health_impact_data.csv")

# Convert date to proper format
# air_data$date <- as.Date(air_data$date)

# Check data structure
str(air_data)
summary(air_data)

# Check for missing values
missing_values <- sapply(air_data, function(x) sum(is.na(x)))
print(missing_values)

# Boxplot of pollutant levels
ggplot(gather(air_data, key = "Pollutant", value = "Value", 
              AQI, PM10, PM2_5, NO2, SO2, O3), 
       aes(x = Pollutant, y = Value)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Distribution of Air Pollutants",
       y = "Concentration",
       x = "Pollutant") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Correlation analysis
numeric_data <- air_data %>%
  select(AQI, PM10, PM2_5, NO2, SO2, O3, 
         Temperature, Humidity, WindSpeed,
         RespiratoryCases, CardiovascularCases, 
         HospitalAdmissions, HealthImpactScore)

correlation_matrix <- cor(numeric_data, use = "complete.obs")
corrplot(correlation_matrix, method = "circle", 
         order = "hclust", tl.col = "black", tl.srt = 45)

# Relationship between AQI and health metrics
ggplot(air_data, aes(x = AQI, y = RespiratoryCases)) +
  geom_point(aes(color = factor(HealthImpactClass)), size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  theme_minimal() +
  labs(title = "AQI vs Respiratory Cases",
       x = "Air Quality Index",
       y = "Number of Respiratory Cases",
       color = "Health Impact Class")


# 5. Time Series Analysis
# Note: For proper time series analysis, we need date information
# We'll use RecordID as a proxy if date isn't available

# Convert data to time series
# aqi_ts <- ts(air_data$AQI)
# print(aqi_ts)
# pm25_ts <- ts(air_data$PM2_5)


# Plot AQI time series (using RecordID as proxy for time)
ggplot(air_data, aes(x = RecordID, y = AQI)) +
  geom_line(color = "steelblue") +
  theme_minimal() +
  labs(title = "AQI Trend",
       x = "Record ID",
       y = "Air Quality Index")

# 8. Health Impact Analysis
# Analyze relationship between pollutants and health impacts
model <- lm(RespiratoryCases ~ AQI + PM2_5 + NO2 + Temperature + Humidity, data = air_data)
summary(model)

# Visualize relationship between predicted and actual health impacts
air_data$predicted_resp_cases <- predict(model)

ggplot(air_data, aes(x = predicted_resp_cases, y = RespiratoryCases)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Actual vs Predicted Respiratory Cases",
       x = "Predicted Cases",
       y = "Actual Cases")

# Convert your air quality index data to a time series object
aqi_ts <- ts(air_data$AQI)

# Check if data is stationary (required for ARIMA)
adf.test(aqi_ts)  # p-value < 0.05 suggests stationarity

# Let R automatically find the best ARIMA parameters
aqi_arima <- auto.arima(aqi_ts)

# View the model details
summary(aqi_arima)  # Shows ARIMA(p,d,q) and accuracy metrics

# Make forecasts for future periods
aqi_forecast <- forecast(aqi_arima, h=10)  # Forecast 10 time periods ahead
print(aqi_forecast)

# Plot the forecast
plot(aqi_forecast)  # Shows forecast with confidence intervals