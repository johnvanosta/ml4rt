rm(list = ls())

pacman::p_load(readxl,ggplot2,RColorBrewer,dplyr,readr,writexl)

####### Structure the location fingerprinting output
# Set folder containing the CSV files
folder_path_lf <- "ml4rt_output/predictions/sample_size_analysis_fingerprinting"

# List all CSV files in the folder
file_list_lf <- list.files(path = folder_path_lf, pattern = "\\.csv$", full.names = TRUE)

# Read and store all data frames in a list
dataframes <- lapply(file_list_lf, read_csv)

# Combine all dataframes into a single dataframe
sample_size_data_lf <- bind_rows(dataframes)

# Export the dataframe to an Excel file
#write_xlsx(sample_size_data_lf, "ml4rt_output/predictions/sample_size_analysis_fingerprinting.xlsx")


####### Structure the Linear regression output
# Set folder containing the xlsx files
folder_path_lr <- "ml4rt_output/predictions/sample_size_analysis_angulation_w_distance"

# List all xlsx files in the folder
file_list_lr <- list.files(path = folder_path_lr, pattern = "\\.xlsx$", full.names = TRUE)

# Initialize an empty dataframe to store results
results_df <- data.frame(n = integer(), r = integer(), mean_error = numeric(), std_error = numeric())

# Iterate through the files to create a new dataframe
for (file in file_list_lr) {
  # Get the filename and split the first two values denominated by "_".
  file_name_parts <- unlist(strsplit(basename(file), "_"))
  n_value <- as.integer(file_name_parts[1])
  r_value <- as.integer(file_name_parts[2])

    # Open the xlsx file and read the 'error_m' column
  error_m_data <- read_excel(file)
  
  # Calculate the average and standard error of the 'error_m' column
  mean_error <- mean(error_m_data$error_m)
  std_error <- sd(error_m_data$error_m) / sqrt(length(error_m_data$error_m))
  
  # Combine the data into a new row and add to the results dataframe
  results_df <- rbind(results_df, data.frame(r = r_value, n = n_value, mean_error = mean_error, std_error = std_error))
}

# View the final dataframe
print(results_df)

# Export the dataframe to an Excel file
#write_xlsx(results_df, "ml4rt_output/predictions/sample_size_analysis_ang_w_dist.xlsx")

# Import data
lf_data <- read_excel("ml4rt_output/predictions/sample_size_analysis_fingerprinting/sample_size_analysis_fingerprinting.xlsx")
lr_data <- read_excel("ml4rt_output/predictions/sample_size_analysis_angulation_w_distance/sample_size_analysis_ang_w_dist.xlsx")

# Add a 'method' column to each data frame
lf_data <- lf_data %>% mutate(method = "Location fingerprinting")
lr_data <- lr_data %>% mutate(method = "Linear regression")

sample_size_data <- bind_rows(lf_data, lr_data)

# Set the color scheme
colors <- brewer.pal(8, "Set2")  # Adjust the number if needed

# Plot sample size against model performance, including a trend line, and facet by 'method'
plot1 <- ggplot(sample_size_data, aes(x = n, y = mean_error, color = method)) +
  geom_point(alpha = 0.6) +
  labs(x = "Number of training samples", y = "Positional error (m)") +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_color_manual(values = colors) +
  facet_wrap(~ method)

# Print the plot
print(plot1)


# Summarize the data
summarized_data <- sample_size_data %>%
  group_by(n, method) %>%
  summarize(
    avg_mean_error = mean(mean_error),
    sd_error = sd(mean_error),
    count = n(),
    se_error = sd_error / sqrt(count),
    lower_ci = avg_mean_error - qt(0.975, count - 1) * se_error,
    upper_ci = avg_mean_error + qt(0.975, count - 1) * se_error
  )

plot2 <- ggplot(summarized_data, aes(x = n, y = avg_mean_error, group = method)) +
  geom_errorbar(aes(ymin = avg_mean_error - sd_error, ymax = avg_mean_error + sd_error), width = 0.2, color = "black") +  # Error bars representing standard deviation
  geom_point(color = "black", size = 1) +  # Add points in black
  labs(x = "Number of training samples", y = "Positional error (m)") +
  theme_bw() + 
  coord_cartesian(xlim = c(0, 200), ylim = c(300, 600)) + 
  theme(
    text = element_text(size = 12),
    legend.position = "none"  # Remove the legend
  ) +
  facet_wrap(~ method)

print(plot2)

#ggsave("Paper_results/Figures/positional_error_sample_size_20240401.png", plot = plot2,
       width = 140, height = 90, units = "mm", dpi = 600)

