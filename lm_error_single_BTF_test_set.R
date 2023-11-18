pacman::p_load(readxl,PerformanceAnalytics,stats,tidyverse,report,parameters,jtools,interactions,broom,gridExtra,RESI,DHARMa,sjPlot)

excel_path <- "Example_data/Output/Predictions/UTM_predictions_combined_dtypes_input_20230701.xlsx"

predict_df <- read_excel(excel_path)

predict_df$log_error_m <- log(predict_df$error_m, base = exp(1))
predict_df$log_mean_distance_from_tower <- log(predict_df$mean_distance_from_tower, base = exp(1))
predict_df$inteval_seconds <- as.factor(predict_df$inteval_seconds)

# Filter data to predictions on tracked birds (excluding simulated birds)
predict_df <- predict_df[predict_df$Data_type == 'Tracked bird', ]

head(predict_df)

#predict_df$inteval_seconds <- as.factor(predict_df$inteval_seconds)
model1 <- lm(log_error_m ~ training_data_type + Tower_count +
               inteval_seconds + log_mean_distance_from_tower * training_data_type + Power, 
             data = predict_df)

summary(model1)
broom(model1)
parameters(model1)
summ(model1)
resi.obj = resi(model1, nboot = 100, store.boot = TRUE, alpha = 0.05)
anova(resi.obj, alpha = 0.05)
simulationOutput <- simulateResiduals(fittedModel = model1, plot = F)
plot(simulationOutput)


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

# Set consistent y label
yl = expression(bold("log"[e]*"Positional error (m)"))
pos_error_label = expression(bold("log"[e]*"Mean distance to tower (m)"))

theme_pub <- function() {
  theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(size = 0.5),
          legend.title = element_text(size = 9, face = "bold"),
          legend.text = element_text(size = 8),
          legend.key.size = unit(0.4, "cm"),
          legend.key.height = unit(0.3, "cm"),
          legend.background = element_rect(fill = "transparent"),
          legend.position = "top",
          axis.title = element_text(size = 9, face = "bold"),
          axis.text = element_text(size = 8),
          plot.title = element_text(size = 10, face = "bold", hjust = 0.5))
}

# Plots for interactions
## Plot1
plot1 <- interact_plot(model1, pred = log_mean_distance_from_tower, 
              modx = training_data_type, 
              plot.points=TRUE,
              point.size = 0.5,
              partial.resdiuals=TRUE, 
              interval=FALSE, int.width = 0.95,
              jitter = 0.2,
              x.label = pos_error_label,
              y.label = yl,
              legend.main = "Training dataset",
              line.thickness = 0.8)

plot1 <- plot1 +
  theme_pub() +
  theme(legend.position = c(0.8, 0.15), legend.key.size = unit(1.3,"line")) +
  labs(tag = "(B)") +
  theme(plot.tag = element_text(size = 15)) +
  scale_x_continuous(limits = c(3, 8), breaks = seq(0, 10, 1))

plot(plot1)

## Plot2
plot2 <- effect_plot(model1, pred = Power,
            interval = FALSE, 
            plot.points = TRUE, 
            partial.residuals = TRUE,
            x.label = "Mean signal strength (RSSI)",
            y.label = yl,
            jitter = 0.15)

plot2 <- plot2 +
  theme_pub() +
  labs(tag = "(C)") +
  theme(plot.tag = element_text(size = 15))

plot(plot2)

## Plot3
plot3 <- effect_plot(model1, pred = inteval_seconds,
            interval = TRUE,
            int.width = 0.95,
            partial.residuals = FALSE,
            x.label = "Tag interval (seconds)",
            y.label = yl)

plot3 <- plot3 +
  theme_pub() +
  labs(tag = "(D)") +
  theme(plot.tag = element_text(size = 15))

plot(plot3)

## Plot4
plot4 <- cat_plot(model1, pred = training_data_type,
         plot.points = FALSE,
         interval = TRUE,
         int.width = 0.95,
         partial.residuals = FALSE,
         x.label = "Training dataset",
         y.label = yl,
         legend.main = "Testing dataset",
         line.thickness = 0.8)

plot4 <- plot4 +
  theme_pub() +
  labs(tag = "(A)") +
  theme(plot.tag = element_text(size = 15), legend.position = c(0.22, 0.95))

plot4 <- plot4 + scale_y_continuous(scale_x_continuous(trans = "log10"))

plot(plot4)

## # Arrange subplots in a grid layout with labels
plot_grid <- grid.arrange(plot4, plot1, plot2, plot3, ncol=2, nrow=2)
plot(plot_grid)

ggsave("Paper_results/Figures/positional_error_covariates_20231118.png", plot = plot_grid,
       width = 170, height = 170, units = "mm", dpi = 300)


############ Summary stats
# Group by Data_type and training_data_type and calculate the mean and standard error
summary_stats <- predict_df %>%
  group_by(Data_type, training_data_type) %>%
  summarize(
    mean_error_m = mean(error_m),
    std_error_m = sd(error_m) / sqrt(n())
  )

# Print the summary statistics
print(summary_stats)

# Filter the dataframe to include only "Combined" training_data_type
combined_data <- predict_df %>%
  filter(training_data_type == "Combined")

# Calculate the mean and standard error for "error_m" column
mean_error_combined <- mean(combined_data$error_m)
std_error_combined <- sd(combined_data$error_m) / sqrt(nrow(combined_data))

# Print the results
cat("Mean of error_m for training_data_type == 'Combined':", mean_error_combined, "\n")
cat("Standard error of the mean for training_data_type == 'Combined':", std_error_combined, "\n")



##########################################
## Alternative removing simulated birds from the test data
#########################################

# Filter the dataframe to include only "Tracked bird" rows
filtered_predict_df <- predict_df %>%
  filter(Data_type == "Tracked bird")

head(filtered_predict_df)

#predict_df$inteval_seconds <- as.factor(predict_df$inteval_seconds)
model2 <- lm(log_error_m ~ training_data_type + Tower_count +
               inteval_seconds + mean_distance_from_tower * training_data_type, 
             data = predict_df)

summary(model2)

plot5 <- cat_plot(model2, pred = training_data_type,
                  plot.points = FALSE,
                  interval = TRUE,
                  int.width = 0.95,
                  x.label = "Training dataset",
                  y.label = yl,
                  legend.main = "Testing dataset",
                  line.thickness = 0.8)
plot(plot5)

### try to make a prediction
new = data.frame(Data_type = c("Simulated bird"),
                 training_data_type = c("Combined"),
                 Tower_count = c(3),
                 inteval_seconds = c("13"),
                 log_mean_distance_from_tower = c(6.011))
exp(predict(model1, newdata=new))



### working

## Plot1
# Create facet wrap plots by training data type
plot1 <- interact_plot(model2, 
                       pred = log_mean_distance_from_tower, 
                       modx = training_data_type, 
                       plot.points = TRUE,
                       point.size = 0.5,
                       partial.residuals = TRUE, 
                       interval = FALSE, int.width = 0.95,
                       jitter = 0.2,
                       x.label = "Mean distance to tower (m)",
                       y.label = yl,
                       legend.main = "Training dataset",
                       line.thickness = 0.8) +
  theme_pub() +
  theme(legend.position = c(0.8, 0.15), legend.key.size = unit(1.3,"line")) +
  labs(tag = "(B)") +
  theme(plot.tag = element_text(size = 15)) +
  facet_wrap(~ training_data_type, ncol = 2)  # Adjust the number of columns as needed

plot(plot1)

plot_model(model1, line.color = "red", sort.est = TRUE, type = "std", show.values = TRUE, value.offset = .3)

