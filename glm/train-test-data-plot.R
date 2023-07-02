library(readxl)
library(ggplot2)

excel_path <- "C:/Users/s5236256/Documents/GitHub/ml4rt/Example_data/Output/Predictions/UTM_predictions_combined_dtypes_input_20230701.xlsx"

predict_df <- read_excel(excel_path)

head(predict_df)

predict_df$log_error_m <- log(predict_df$error_m, base = 10)

# Set custom theme for publication-ready plot
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

# Create a grouped boxplot
ggplot(data = predict_df, aes(x = training_data_type, y = log_error_m, fill = Data_type)) +
  geom_boxplot() +
  labs(x = "Training dataset", y = expression("log"[10]*"(locational error (m))")) +
  scale_fill_discrete(name = "Testing dataset") +
  theme_bw() +
  theme(legend.position = "top")

# create xyplots for discussion

# Error vs. mean distance from tower
ggplot(data = predict_df, aes(x = mean_distance_from_tower, y = log_error_m)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Mean Distance from Tower",
       y = expression("log"[10]*"(Locational Error (m))")) +
  theme_pub()

# Error vs. tag interval boxplot
### Convert interval_seconds to a categorical variable
predict_df$inteval_seconds <- factor(predict_df$inteval_seconds)

### Plot
ggplot(data = predict_df, aes(x = inteval_seconds, y = log_error_m)) +
  geom_boxplot() +
  labs(x = "Tag interval (seconds)",
       y = expression("log"[10]*"(Locational Error (m))")) +
  theme_pub()

# Error vs. number of towers
ggplot(data = predict_df, aes(x = Tower_count, y = log_error_m)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Number of towers",
       y = expression("log"[10]*"(Locational Error (m))")) +
  theme_pub()

# Error vs. signal_count
ggplot(data = predict_df, aes(x = Signal_count, y = log_error_m)) +
  geom_point() + 
  theme_bw()

######## Graphs above copied below for input into a combined figure with subplots
###### Print to 400 x 800
library(gridExtra)
library(grid)

# Create subplot (a): Error vs. mean distance from tower
plot_a <- ggplot(data = predict_df, aes(x = mean_distance_from_tower, y = log_error_m)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Mean Distance from Tower",
       y = expression("log"[10]*"(Locational Error (m))")) +
  theme_pub()

# Create subplot (b): Error vs. tag interval boxplot
# Convert interval_seconds to a categorical variable
predict_df$inteval_seconds <- factor(predict_df$inteval_seconds)

plot_b <- ggplot(data = predict_df, aes(x = inteval_seconds, y = log_error_m)) +
  geom_boxplot() +
  labs(x = "Tag interval (seconds)",
       y = expression("log"[10]*"(Locational Error (m))")) +
  theme_pub()

# Create subplot (c): Error vs. number of towers
plot_c <- ggplot(data = predict_df, aes(x = Tower_count, y = log_error_m)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Number of towers",
       y = expression("log"[10]*"(Locational Error (m))")) +
  theme_pub()

# Arrange subplots in a grid layout with labels
grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 1, heights = unit(c(1, 1, 1), "null"))))
print(plot_a, vp = viewport(layout.pos.row = 1))
print(plot_b, vp = viewport(layout.pos.row = 2))
print(plot_c, vp = viewport(layout.pos.row = 3))