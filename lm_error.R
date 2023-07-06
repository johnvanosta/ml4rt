pacman::p_load(readxl,PerformanceAnalytics,stats,tidyverse,report)

excel_path <- "Example_data/Output/Predictions/UTM_predictions_combined_dtypes_input_20230701.xlsx"

predict_df <- read_excel(excel_path)

predict_df$log_error_m <- log(predict_df$error_m, base = 10)

head(predict_df)

# Create pairwise plots
pairwise_columns <- c("Tower_count", "inteval_seconds", "mean_distance_from_tower", "Signal_count")
chart.Correlation(predict_df[,pairwise_columns],pch=20,histogram=TRUE,method="pearson")

#predict_df$inteval_seconds <- as.factor(predict_df$inteval_seconds)

model1 <- lm(log_error_m ~ Data_type * training_data_type + Tower_count +
               inteval_seconds + mean_distance_from_tower * training_data_type, 
             data = predict_df)

summary(model1)

# Residual plot for homoscedasticity
plot(fitted(model1), residuals(model1), main = "Residuals vs. Fitted Values", xlab = "Fitted Values", ylab = "Residuals")

# Histogram or Q-Q plot for normality of residuals
hist(residuals(model1), main = "Histogram of Residuals")
# Alternatively, use qqnorm and qqline functions for Q-Q plot
qqnorm(residuals(model1))
qqline(residuals(model1))



# Load the model into papeR
report(model1)
report_table(model1)


# Capture the summary output as character strings
summary_output <- capture.output(summary(model1))

# Print the captured output
print(summary_output)

# Write the captured output to a text file
writeLines(summary_output, "linear_model_summary.txt")

# Convert the summary output to a data frame
summary_df <- data.frame(summary_output)

# Write the data frame to a CSV file
write.csv(summary_df, "linear_model_summary.csv", row.names = FALSE)

