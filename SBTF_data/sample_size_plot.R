# Load libraries
pacman::p_load(readxl,ggplot2,RColorBrewer,dplyr,readr)

# Set folder containing the CSV files
folder_path <- "paper_results/sample_size_trial/individual_runs"

# List all CSV files in the folder
file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Read and store all data frames in a list
dataframes <- lapply(file_list, read_csv)

# Combine all dataframes into a single dataframe
sample_size_data <- bind_rows(dataframes)

# Import data
#excel_path <- "C:/Users/s5236256/Documents/GitHub/ml4rt/Paper_results/Sample_size_trial/sample_size_trial_20230717.xlsx"
#sample_size_data <- read_excel(excel_path)

# Set the color scheme
colors <- brewer.pal(8, "Set2")  # Adjust the number if needed

# Plot sample size against model performance, including a trend line
plot1 <- ggplot(sample_size_data, aes(x = n, y = mean_error)) +
  geom_point(color = "black", alpha = 0.6) +
  labs(x = "Number of training samples", y = "Positional error (m)") +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  geom_smooth(method = "loess", se = FALSE, color = colors[3])  # Use the first color from the palette

# Apply the color scheme to the plot
plot1 + scale_color_manual(values = colors)

#ggsave("Paper_results/Figures/positional_error_sample_size_20230813.png", plot = plot1,
       width = 85, height = 85, units = "mm", dpi = 300)
