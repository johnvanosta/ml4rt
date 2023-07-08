# Load libraries
pacman::p_load(readxl,ggplot2)

# Import data
excel_path <- "Paper_results/Sample_size_trial/sample_size_trial_20230708.xlsx"
sample_size_data <- read_excel(excel_path)

# Plot sample size against model performance, including a trend line
ggplot(sample_size_data, aes(x = n, y = mean_error)) +
  geom_point(color = "black", alpha = 0.6) +
  labs(x = "Number of training samples", y = "Mean absolute error (m)") +
  theme_minimal() +
  theme(text = element_text(size = 12)) +
  geom_smooth(method = "nls", formula = y ~ exp(-x), se = FALSE)

# Plot sample size against model performance, including a trend line
ggplot(sample_size_data, aes(x = n, y = mean_error)) +
  geom_point(color = "black", alpha = 0.6) +
  labs(x = "Number of training samples", y = "Mean absolute error (m)") +
  theme_minimal() +
  theme(text = element_text(size = 12)) +
  geom_smooth(method = "loess", se = TRUE)
