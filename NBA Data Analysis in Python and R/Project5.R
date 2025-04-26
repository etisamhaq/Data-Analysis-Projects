# Import data in R
library(corrplot)
library(ggplot2)

nba_data <- read.csv("PlayerStatistics.csv")

# Basic data cleaning
# Converts the game date column to a proper date format
nba_data$gameDate <- as.Date(nba_data$gameDate)

# make player name column
nba_data$playerName <- paste(nba_data$firstName, nba_data$lastName)

# Identifies all the numeric columns (like points, assists, etc.)

str(nba_data)
numeric_cols <- c("numMinutes", "points", "assists", "blocks", "steals", 
                  "fieldGoalsMade", "fieldGoalsAttempted", "threePointersMade", 
                  "threePointersAttempted", "freeThrowsMade", "freeThrowsAttempted",
                  "reboundsDefensive", "reboundsOffensive", "reboundsTotal")


# Convert to numeric and handle NAs
for(col in numeric_cols) {
  nba_data[[col]] <- as.numeric(as.character(nba_data[[col]]))
  nba_data[[col]][is.na(nba_data[[col]])] <- 0
}

# Correlation analysis between key metrics
cor_metrics <- c("points", "assists", "blocks", "steals", "reboundsTotal", 
                 "fieldGoalsPercentage", "threePointersPercentage", "freeThrowsPercentage")

cor_matrix <- cor(nba_data[cor_metrics], use = "complete.obs")
print("Correlation between performance metrics:")
print(cor_matrix)
corrplot(cor_matrix, method = "circle")

# Linear regression: Predict points based on other metrics
model <- lm(points ~ assists + blocks + steals + reboundsTotal + 
              fieldGoalsPercentage + threePointersPercentage + freeThrowsPercentage, 
            data = nba_data)

print(model)

summary(model)
print("Key factors that contribute to scoring:")
print(summary(model)$coefficients)

# Performance comparison in wins vs losses

t_test_result <- t.test(points ~ win, data = nba_data)
print("Difference in scoring between wins and losses:")
print(t_test_result)

# This supports the idea that higher individual scoring is associated with 
# team wins, even if it's a relatively small difference.


# Check for correlations between minutes played and other metrics
minutes_cor <- cor(nba_data$numMinutes, nba_data[numeric_cols], use = "complete.obs")
print("Correlation between minutes played and performance:")
print(minutes_cor)


corrplot(minutes_cor, method = "circle")

# Create player performance trend visualization
# For this example, let's track a specific player across games

player_data <- subset(nba_data, playerName == "Tyler Herro")
player_data <- player_data[order(player_data$gameDate),]

ggplot(player_data, aes(x = gameDate, y = points)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 3) +
  labs(title = "Tyler Herro Scoring Trend", 
       x = "Game Date", 
       y = "Points") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave("player_trend_r.png")


# Performance metrics distribution
ggplot(nba_data, aes(x = points)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Points Scored",
       x = "Points", 
       y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave("points_distribution_r.png")