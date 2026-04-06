library(dplyr)
library(ggplot2)

# ---- LOAD DATA ----
source("R/load_data.R")
data <- load_and_prepare_data()

# ---- SUMMARY ----
summary(data$revenue)
summary(data$profit)
summary(data$margin)

# ---- DISTRIBUTIONS ----
ggplot(data, aes(revenue)) +
  geom_histogram(fill = "#8B4513", bins = 30) +
  labs(title = "Distribution du chiffre d'affaires") +
  theme_minimal()

ggplot(data, aes(profit)) +
  geom_histogram(fill = "darkgreen", bins = 30) +
  labs(title = "Distribution du profit") +
  theme_minimal()

# ---- RELATION REVENUE / PROFIT ----
ggplot(data, aes(revenue, profit)) +
  geom_point(alpha = 0.3) +
  labs(title = "Relation Revenue vs Profit") +
  theme_minimal()

# ---- CORRELATION ----
cor(data$revenue, data$profit)
