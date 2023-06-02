# Install the required R package
# install.packages("readxl")
# install.packages("dplyr")

# Load the required packages
library(readxl)
library(dplyr)
library(ggplot2)

# Read the Excel sheet into a data frame
utm_predictions <- read_excel("UTM_predictions.xlsx")

# Define the model formula
formula <- error_m ~ mean_distance_from_tower + Tower_count + Data_type + pulse_count

# Fit the generalized linear model
model <- glm(formula, data = utm_predictions, family = gaussian())

# Print the summary of the model
summary(model)

plot(model)
