## BRONCO WINE COMPANY SIMULATION

#clear environment
rm(list=ls())

#load packages
library(tidyverse)
library(fpp3)
library(triangle)
library(ggplot2)

#set parameters
set.seed(111) #reproducibility 
n <- 10000
demand_min <- 500
demand_most_likely <- 1500
demand_max <- 2500
fixed_cost <- 10000
cost_per_bottle <- 15
price_per_bottle <- 100
salvage_value_per_bottle <- 30
salvage_limit <- 1000 
alpha <- 2
beta <- 5
max_capacity <- 3000

#production targets
target <- seq(1000, 3000, by=500)

#demand and capacity simulations
demand <- rtriangle(n, demand_min, demand_max, demand_most_likely)
capacity <- rbeta(n, alpha, beta) * max_capacity

#calculate the profit
profit <- function(production) {
  sales <- pmin(demand, production)
  unsold <- pmax(0, production - demand)
  salvage <- pmin(unsold, salvage_limit) * salvage_value_per_bottle
  revenue <- sales * price_per_bottle
  total_cost <- fixed_cost + production * cost_per_bottle
  profit <- revenue + salvage - total_cost
  return(profit)
}

#perform function on targets and calculate average profits
profits <- sapply(target, function(production) {
  valid_production <- pmin(production, capacity)  # Limit by capacity
  profit <- profit(valid_production)
  return(mean(profit))
})

#optimal production level
optimal_production <- target[which.max(profits)]
#optimal profits
optimal_profits <- profit(pmin(optimal_production, capacity))

#stats
print(optimal_production)
summary(optimal_profits)

#plot
ggplot(tibble(profits = optimal_profits), aes(x = profits)) +
  geom_histogram(binwidth = 500, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(profits)), color = "red", linetype = "dashed", size = 1) +
  labs(
    title = paste("Profit Distribution at Optimal Production Level:", optimal_production, "Bottles"),
    x = "Profit ($)",
    y = "Frequency",
    caption = "Dashed red line represents the average profit."
  ) +
  theme_minimal()
