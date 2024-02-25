# Code to compare the performance of each version of the model (listed in the paper)

rm(list = ls())

pacman::p_load(readxl,openxlsx,dplyr,ggplot2, viridis, boot, glmmTMB, DHARMa, sjPlot, gridExtra, broom.mixed, stringr, tidyr,MuMIn)

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

######### glmm data preparation

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
    tower_count = if_else(model %in% c("m4", "m5") & is.na(Tower_count), ref_tower_count, Tower_count),
    Signal_count = if_else(model %in% c("m4", "m5") & is.na(Signal_count), ref_Signal_count, Signal_count),
    mean_distance_from_tower = if_else(model %in% c("m4", "m5") & is.na(mean_distance_from_tower), ref_mean_distance_from_tower, mean_distance_from_tower)
  ) %>%
  select(-starts_with("ref_")) # Remove reference columns

# Get mean signal strength and tag interval
variables_to_merge <- read_excel("paper_results/figures/mean_sig_strength_tag_intervals.xlsx", sheet = 'summary')

# Joining variables_to_merge into combined_df based on "DateTime" and "TagID"
combined_df <- combined_df %>%
  left_join(variables_to_merge, by = c("DateTime", "TagID")) %>%
  mutate(mean_distance_from_tower_km = mean_distance_from_tower / 1000)

# show distribution of error_m
ggplot(combined_df, aes(x = error_m)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  labs(title = "Histogram of error_m", x = "error_m", y = "Count") +
  theme_minimal()

# Relabel methods and set the order
combined_df$method_name <- factor(combined_df$model,
                                  levels = c("m1", "m2", "m3", "m4", "m5"),
                                  labels = c("Fingerprinting with\ncombined training data", 
                                             "Fingerprinting with\nradio tracked training data",
                                             "Fingerprinting with\nsimulated training data", 
                                             "Angulation with\nintersect", 
                                             "Angulation with\ndistance"))

combined_df <- subset(combined_df, !(model %in% c("m1", "m3")))

# Convert interval to categorical
combined_df$Interval_seconds <- factor(combined_df$Interval_seconds)

# Standardizing and centering predictor variables
combined_df$tower_count_standardised <- scale(combined_df$tower_count)
combined_df$mean_distance_from_tower_km_standardised <- scale(combined_df$mean_distance_from_tower_km)
combined_df$mean_rss_standardised <- scale(combined_df$mean_rss)

### glmm multiple model inference
yl = "Positional error (m)"

null_model <- glmmTMB(error_m ~ 1 +
                        (1 | Point_ID),
                      data = combined_df, family = Gamma(link = "log"))

method_only <- glmmTMB(error_m ~ method_name +
                         (1 | Point_ID),
                       data = combined_df, family = Gamma(link = "log"))

properties_only <- glmmTMB(error_m ~ tower_count_standardised + Interval_seconds +
                             mean_distance_from_tower_km_standardised + mean_rss_standardised +
                         (1 | Point_ID),
                       data = combined_df, family = Gamma(link = "log"))

all_no_interac <- glmmTMB(error_m ~ method_name + tower_count_standardised + Interval_seconds +
                            mean_distance_from_tower_km_standardised + mean_rss_standardised +
                            (1 | Point_ID),
                          data = combined_df, family = Gamma(link = "log"))

all_method_interac <- glmmTMB(error_m ~ method_name * (tower_count_standardised + Interval_seconds +
                            mean_distance_from_tower_km_standardised + mean_rss_standardised) +
                            (1 | Point_ID),
                          data = combined_df, family = Gamma(link = "log"))

model.sel(null_model, method_only, properties_only, all_no_interac, all_method_interac, rank = "AIC")

best_model <- all_method_interac

summary(best_model)

simulationOutput <- simulateResiduals(fittedModel = best_model, plot = F)
plot(simulationOutput)

tidy_model <- broom.mixed::tidy(best_model, conf.int = TRUE) %>%
  select(term, estimate, conf.low, conf.high, p.value)
print(tidy_model)

#write.xlsx(tidy_model, file = "paper_results/Figures/Model_1_glmm_covariates_20240128.xlsx")

# Number of observations
nrow(combined_df)

# R-squared for GLMM
performance::r2(best_model)

### Plot output
theme_set(theme_bw())

# Plot effects
odds_plot <- plot_model(best_model, sort.est = TRUE, vline.color = "black", show.p = TRUE, show.values = TRUE, 
           value.offset = 0.4, value.size = 4, title="")

odds_plot <- odds_plot + 
  scale_x_discrete(expand = expansion(add = c(0.5, 0.8)),
                   labels=list(
                     'method_nameAngulation with\nintersect' = 'Angulation with intersect',
                     'method_nameAngulation with\ndistance' = 'Angulation with distance',
                     'tower_count_standardised' = 'Tower count',
                     'Interval_seconds13' = 'Interval (13 seconds)',
                     'mean_distance_from_tower_km_standardised' = 'Mean distance from tower',
                     'mean_rss_standardised' = 'Mean relative signal strength',
                     'method_nameAngulation with\nintersect:tower_count_standardised' = 'Angulation with intersect x\ntower count',
                     'method_nameAngulation with\ndistance:tower_count_standardised' = 'Angulation with distance x\ntower count',
                     'method_nameAngulation with\nintersect:Interval_seconds13' = 'Angulation with intersect x\ntag interval',
                     'method_nameAngulation with\ndistance:Interval_seconds13' = 'Angulation with distance x\ntag interval',
                     'method_nameAngulation with\nintersect:mean_distance_from_tower_km_standardised' = 'Angulation with intersect x\nmean distance from tower',
                     'method_nameAngulation with\ndistance:mean_distance_from_tower_km_standardised' = 'Angulation with distance x\nmean distance from tower',
                     'method_nameAngulation with\nintersect:mean_rss_standardised' = 'Angulation with intersect x\nmean relative signal strength',
                     'method_nameAngulation with\ndistance:mean_rss_standardised' = 'Angulation with distance x\nmean relative signal strength'
                   )) +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1)))

plot(odds_plot)

odds_plot$data # to get variable names

# Plot methods
plot_method <- plot_model(best_model, type = "eff", terms = c("method_name"),show.legend = FALSE,
                    dot.size = 1.5, line.size = 0.8, title = "",
                    axis.title = c("", yl),
                    colors = 'Set1', show.data = FALSE, dot.alpha = 0.3) +
  coord_flip()
plot(plot_method)

#ggsave("paper_results/Figures/model_comparison_20240221.png", plot = model_comparison_plt, width = 140, height = 80, units = "mm", dpi = 600)

# Plot1
plot1 <- plot_model(best_model, type = "eff", terms = c("mean_distance_from_tower_km_standardised"),show.legend = FALSE,
                    dot.size = 1.5, line.size = 0.8, title = "",
                    axis.title = c("Mean distance from tower", yl), jitter = 0.01,
                    colors = 'Set1', show.data = TRUE, dot.alpha = 0.3) +
  labs(tag = "(A)")
plot(plot1)

# Plot2
plot2 <- plot_model(best_model, type = "eff", terms = c("mean_rss_standardised"),show.legend = FALSE,
                    dot.size = 1.5, line.size = 0.8, title = "",
                    axis.title = c("Mean signal strength (RSSI)", yl), jitter = 0.01,
                    colors = 'Set1', show.data = TRUE, dot.alpha = 0.3) +
  labs(tag = "(B)")
plot(plot2)

# Plot3
plot3 <- plot_model(best_model, type = "eff", terms = "Interval_seconds",show.legend = FALSE,
                    dot.size = 1.5, line.size = 0.8, title = "",
                    axis.title = c("Tag interval (seconds)", yl),
                    colors = 'Set1') +
  labs(tag = "(C)")+ 
  coord_cartesian(xlim=c(0,16))
plot(plot3)

# Plot4
plot4 <- plot_model(best_model, type = "eff", terms = "tower_count_standardised",
                    dot.size = 1.5, line.size = 0.8, title = "",
                    axis.title = c("Tower count", yl), jitter = 0.01,
                    colors = 'Set1', ci.lvl = 0.95, show.data = TRUE, dot.alpha = 0.3) +
  labs(tag = "(D)")
  
plot(plot4)

# Arrange subplots in a grid layout with labels
plot_grid <- grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)

# Save the plot grid
#ggsave("paper_results/Figures/positional_error_covariates_20240128.png", plot = plot_grid, width = 160, height = 160, units = "mm", dpi = 600)

# Interaction plots
plot1 <- plot_model(best_model, type = "eff", terms = c("mean_distance_from_tower_km"),show.legend = FALSE,
                    dot.size = 1.5, line.size = 0.8, title = "",
                    axis.title = c("Mean distance from tower", yl), jitter = 0.1,
                    colors = 'Set1', show.data = TRUE, dot.alpha = 0.3) +
  labs(tag = "(A)")
plot(plot1)



