# Load libraries
pacman::p_load(readxl,ggplot2)

# Import data
excel_path <- "Paper_results/Sample_size_trial/sample_size_trial_20230717.xlsx"
sample_size_data <- read_excel(excel_path)

# Remove rows where 'n' is less than 5
sample_size_data <- sample_size_data[sample_size_data$n >= 5, ]

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

# Set the color scheme
colors <- brewer.pal(8, "Set2")  # Adjust the number if needed

# Plot sample size against model performance, including a trend line
plot1 <- ggplot(sample_size_data, aes(x = n, y = mean_error)) +
  geom_point(color = "black", alpha = 0.6) +
  labs(x = "Number of training samples", y = "Locational error (m)") +
  theme_pub() +
  theme(text = element_text(size = 12)) +
  geom_smooth(method = "loess", se = FALSE, color = colors[3])  # Use the first color from the palette

# Apply the color scheme to the plot
plot1 + scale_color_manual(values = colors)

ggsave("Paper_results/Figures/positional_error_sample_size_20230717.png", plot = plot1,
       width = 85, height = 85, units = "mm", dpi = 300)
