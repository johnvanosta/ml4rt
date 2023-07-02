library(readxl)
library(stats)
library(tidyverse)

excel_path <- "C:/Users/s5236256/Documents/GitHub/ml4rt/Example_data/Output/Predictions/UTM_predictions_combined_dtypes_input_20230701.xlsx"

predict_df <- read_excel(excel_path)

predict_df$log_error_m <- log(predict_df$error_m, base = 10)

head(predict_df)

#### When do I need to mutate or log data?

model <- glm(log_error_m ~ Data_type + training_data_type + Tower_count + Signal_count +
               inteval_seconds + mean_distance_from_tower, 
             data = predict_df, family = gaussian())

summary(model)

# Plot the residuals
plot(model$residuals, pch = 16, main = "Residuals Plot", xlab = "Observation", ylab = "Residuals")

# Add a horizontal line at y = 0 for reference
abline(h = 0, col = "red")

model <- lm(log_error_m ~ Tower_count +
               inteval_seconds + mean_distance_from_tower, 
             data = predict_df)

summary(model)

# Try an anova
model <- aov(log_error_m ~ Data_type * training_data_type, data = predict_df)
summary(model)

anova_result <- anova(model)
print(anova_result)

# Extract the residuals from the model
residuals <- residuals(model)

# Plot the residuals
plot(residuals, pch = 16, main = "Residuals Plot", xlab = "Observation", ylab = "Residuals")
