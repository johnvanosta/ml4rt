# Code to compare the performance of each version of the model (listed in the paper)

rm(list = ls())

pacman::p_load(readxl,openxlsx,dplyr,ggplot2, viridis, boot, glmmTMB, DHARMa, sjPlot, gridExtra, broom.mixed, stringr, tidyr)

m1 <- read_excel("ml4rt_output/trained_models/m1/error_estimates.xlsx")
m2 <- read_excel("ml4rt_output/trained_models/m2/error_estimates.xlsx")
m3 <- read_excel("ml4rt_output/trained_models/m3/error_estimates.xlsx")
m4 <- read_excel("ml4rt_output/trained_models/m4/error_estimates_mech_w_biangulation.xlsx")
m5 <- read_excel("ml4rt_output/trained_models/m5/error_estimates_mech_w_linear_regression.xlsx")

# Adding an identifier column to each dataframe
m1$model <- "m1"
m2$model <- "m2"
m3$model <- "m3"
m4$model <- "m4"
m5$model <- "m5"

# Remove rows where 'error_m' is na or greater than 10000 for the biangulation method
m4 <- m4 %>%
  filter(!is.na(error_m)) %>%
  filter(error_m <= 10000)

# Combining the dataframes
combined_df <- bind_rows(m1, m2, m3, m4, m5)

summary_df <- combined_df %>%
  group_by(model) %>%
  summarise(
    median_error_m = median(error_m, na.rm = TRUE),
    mean_error_m = mean(error_m, na.rm = TRUE)
  )

# Print the summary dataframe
print(summary_df)

mean(combined_df$mean_distance_from_tower, na.rm = TRUE)

# Create the violin plot of model error
ggplot(combined_df, aes(x = model, y = error_m, fill = model)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, color = "grey", alpha = 0.2, position = position_dodge(width = 0.9)) +
  scale_fill_viridis(discrete = TRUE, option = "D") +
  scale_y_log10() +
  labs(x = "Model", y = "Positional error (m)") +
  theme_bw() +
  theme(legend.position = "none")

### Plot median with 95% confidence intervals (bootstrapped)
# Function to calculate the median
median_fun <- function(data, indices) {
  d <- data[indices]  # allows bootstrapping to select sample
  return(median(d))
}

model_stats <- combined_df %>%
  group_by(model) %>%
  do({
    # Filter out NA values from 'error_m' within the pipeline
    non_na_data <- na.omit(.$error_m)
    
    # Ensure there is enough data for calculations
    if (length(non_na_data) > 1) {
      boot_median <- boot(non_na_data, median_fun, R = 1000)
      median_val <- median(non_na_data, na.rm = TRUE)
      ci <- boot.ci(boot_median, type = "perc")$percent[4:5]
      data.frame(model = first(.$model), median = median_val, ci_lower = ci[1], ci_upper = ci[2])
    } else {
      # Handle the case with insufficient data
      data.frame(model = first(.$model), median = NA, ci_lower = NA, ci_upper = NA)
    }
  }) %>%
  ungroup()

# Create the plot
model_plot <- ggplot(model_stats, aes(x = model, y = median)) +
  geom_point(size = 4, color = "black") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(x = "Model", y = "Median positional error (m)") +
  theme_bw()

plot(model_plot)

# Save the plot as a PNG file
#ggsave("paper_results/figures/model_comparison.png", model_plot, width = 14, height = 11, units = "cm", dpi = 600)


######### glmm to test for causes of differences in model results

# Fill tower count, signal count and mean distance from tower for the mechanistic models using the data from the ML pipeline

# Create a reference dataset from model 'm1'
reference_data <- combined_df %>%
  filter(model == "m1") %>%
  group_by(DateTime, TagID) %>%
  summarise(
    ref_tower_count = first(Tower_count),
    ref_Signal_count = first(Signal_count),
    ref_mean_distance_from_tower = first(mean_distance_from_tower)
  )

# Join the reference data with the original data for model 'm5' and 'm6'
combined_df <- combined_df %>%
  left_join(reference_data, by = c("DateTime", "TagID")) %>%
  mutate(
    tower_count = if_else(model %in% c("m5", "m6") & is.na(Tower_count), ref_tower_count, Tower_count),
    Signal_count = if_else(model %in% c("m5", "m6") & is.na(Signal_count), ref_Signal_count, Signal_count),
    mean_distance_from_tower = if_else(model %in% c("m5", "m6") & is.na(mean_distance_from_tower), ref_mean_distance_from_tower, mean_distance_from_tower)
  ) %>%
  select(-starts_with("ref_")) # Remove reference columns

# Get mean signal strength and tag interval

variables_to_merge <- read_excel("paper_results/figures/mean_sig_strength_tag_intervals.xlsx", sheet = 'summary')

# Joining variables_to_merge into combined_df based on "DateTime" and "TagID"
combined_df <- combined_df %>%
  left_join(variables_to_merge, by = c("DateTime", "TagID"))

# show distribution of error_m
ggplot(combined_df, aes(x = error_m)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  labs(title = "Histogram of error_m", x = "error_m", y = "Count") +
  theme_minimal()

### glmm to assess performance of the different models
yl = "Positional error (m)"

combined_df$method_name <- factor(combined_df$model,
                            levels = rev(c("m1", "m2", "m3", "m4", "m5")),
                            labels = rev(c("Fingerprinting with\ncombined training data", 
                                       "Fingerprinting with\nradio tracked training data",
                                       "Fingerprinting with\nsimulated training data", 
                                       "Angulation with\nintersect", 
                                       "Angulation with\ndistance")))

model_glmm <- glmmTMB(error_m ~ method_name + (1 | Point_ID),
                         data = combined_df, family = nbinom2)

summary(model_glmm)

simulationOutput <- simulateResiduals(fittedModel = model_glmm, plot = F)
plot(simulationOutput)

model_comparison_plt <- plot_model(model_glmm, type = "eff", terms = "method_name",
                    dot.size = 1.5, line.size = 0.8, title = "",
                    axis.title = c("", yl),
                    colors = 'black', ci.lvl = 0.95) +
  theme_bw() +
  coord_flip()

plot(model_comparison_plt)

#ggsave("paper_results/Figures/model_comparison_20240128.png", plot = model_comparison_plt, width = 140, height = 80, units = "mm", dpi = 600)

tidy_model <- broom.mixed::tidy(model_glmm, conf.int = TRUE) %>%
  select(term, estimate, conf.low, conf.high, p.value)
print(tidy_model)

#write.xlsx(tidy_model, file = "paper_results/Figures/Model_glmm_comparison_20240128.xlsx")

# Number of observations
nrow(combined_df)

# R-squared for GLMM
performance::r2(model_glmm)

### glmm for only model 1 to test factors affecting positional error
# Create a dataframe only for m1
m1_data <- combined_df %>%
  filter(model == 'm1') %>%
  mutate(mean_distance_from_tower_km = mean_distance_from_tower / 1000)

# Convert interval to categorical
m1_data$Interval_seconds <- factor(m1_data$Interval_seconds)

no_interac <- glmmTMB(error_m ~ tower_count + Interval_seconds +
                        mean_distance_from_tower_km + mean_rss +
                         (1 | Point_ID),
                       data = m1_data, family = nbinom2)

summary(no_interac)

best_model <- no_interac

simulationOutput <- simulateResiduals(fittedModel = best_model, plot = F)
plot(simulationOutput)

tidy_model <- broom.mixed::tidy(best_model, conf.int = TRUE) %>%
  select(term, estimate, conf.low, conf.high, p.value)
print(tidy_model)

write.xlsx(tidy_model, file = "paper_results/Figures/Model_1_glmm_covariates_20240128.xlsx")

# Number of observations
nrow(m1_data)

# R-squared for GLMM
performance::r2(best_model)

### Plot output

# Plot1
plot1 <- plot_model(best_model, type = "eff", terms = c("mean_distance_from_tower"),show.legend = FALSE,
                    dot.size = 1.5, line.size = 0.8, title = "",
                    axis.title = c("Mean distance from tower", yl),
                    colors = 'Set1', show.data = TRUE, dot.alpha = 0.3) +
  theme_bw() +
  labs(tag = "(A)")
plot(plot1)

# Plot2
plot2 <- plot_model(best_model, type = "eff", terms = c("mean_rss"),show.legend = FALSE,
                    dot.size = 1.5, line.size = 0.8, title = "",
                    axis.title = c("Mean signal strength (RSSI)", yl),
                    colors = 'Set1', show.data = TRUE, dot.alpha = 0.3) +
  theme_bw() +
  labs(tag = "(B)")
plot(plot2)

# Plot3
plot3 <- plot_model(best_model, type = "eff", terms = "Interval_seconds",show.legend = FALSE,
                    dot.size = 1.5, line.size = 0.8, title = "",
                    axis.title = c("Tag interval (seconds)", yl),
                    colors = 'Set1') +
  theme_bw() +
  labs(tag = "(C)")+ 
  coord_cartesian(xlim=c(0,16))
plot(plot3)

# Plot4
plot4 <- plot_model(best_model, type = "eff", terms = "tower_count",
                    dot.size = 1.5, line.size = 0.8, title = "",
                    axis.title = c("Tower count", yl),
                    colors = 'Set1', ci.lvl = 0.95, show.data = TRUE, dot.alpha = 0.3) +
  theme_bw() +
  labs(tag = "(D)")
  
plot(plot4)

# Arrange subplots in a grid layout with labels
plot_grid <- grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)

# Save the plot grid
ggsave("paper_results/Figures/positional_error_covariates_20240128.png", plot = plot_grid, width = 160, height = 160, units = "mm", dpi = 600)






