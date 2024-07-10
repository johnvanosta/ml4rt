rm(list = ls())

pacman::p_load(readxl,ggplot2,RColorBrewer,dplyr,readr,writexl)

# Import data
lf_data <- read_excel("ml4rt_output/sample_size_analysis_location_fingerprinting.xlsx")
lr_data <- read_excel("ml4rt_output/sample_size_analysis_linear_regression.xlsx")

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

#ggsave("Paper_results/Figures/Fig8.jpeg", plot = plot2, width = 140, height = 90, units = "mm", dpi = 300)

