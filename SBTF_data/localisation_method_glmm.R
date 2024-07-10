# Code to compare the performance of each version of the model (listed in the paper)

rm(list = ls())

pacman::p_load(readxl,openxlsx,dplyr,ggplot2, viridis, boot, glmmTMB, DHARMa, sjPlot, gridExtra, broom.mixed, stringr, tidyr,MuMIn,cowplot)

LF <- read_excel("ml4rt_output/trained_models/location_fingerprinting/error_estimates.xlsx")
Biangulation <- read_excel("ml4rt_output/trained_models/angulation_w_intersect/error_estimates_mech_w_biangulation.xlsx")
LR <- read_excel("ml4rt_output/trained_models/angulation_w_distance/error_estimates_mech_w_linear_regression.xlsx")

# Adding an identifier column to each dataframe
LF$model <- "LF"
Biangulation$model <- "Biangulation"
LR$model <- "LR"

# Remove rows where 'error_m' is na or greater than 10000 (i.e. outside of the study area) for the biangulation method
Biangulation <- Biangulation %>%
  filter(!is.na(error_m))

# Combining the dataframes
combined_df <- bind_rows(LF, Biangulation, LR)

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

# Create a reference dataset from model 'LF'
reference_data <- combined_df %>%
  filter(model == "LF") %>%
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
    tower_count = if_else(model %in% c("Biangulation", "LR") & is.na(Tower_count), ref_tower_count, Tower_count),
    Signal_count = if_else(model %in% c("Biangulation", "LR") & is.na(Signal_count), ref_Signal_count, Signal_count),
    mean_distance_from_tower = if_else(model %in% c("Biangulation", "LR") & is.na(mean_distance_from_tower), ref_mean_distance_from_tower, mean_distance_from_tower)
  ) %>%
  select(-starts_with("ref_")) # Remove reference columns

# Get mean signal strength and tag interval
variables_to_merge <- read_excel("paper_results/figures/mean_sig_strength_tag_intervals.xlsx", sheet = 'summary')

# Joining variables_to_merge into combined_df based on "DateTime" and "TagID"
combined_df <- combined_df %>%
  left_join(variables_to_merge, by = c("DateTime", "TagID")) %>%
  mutate(mean_distance_from_tower_km = mean_distance_from_tower / 1000)

# Relabel methods and set the order
combined_df$method_name <- factor(combined_df$model,
                                  levels = c("LF", "Biangulation", "LR"),
                                  labels = c("Location\nfingerprinting",
                                             "Biangulation", 
                                             "Linear\nregression"))

# Convert interval to categorical
combined_df$Interval_seconds <- factor(combined_df$Interval_seconds)

# Export the combined dataframe to an Excel file
#openxlsx::write.xlsx(combined_df, "combined_data.xlsx", overwrite = TRUE)

# Standardizing and centering predictor variables
#combined_df$tower_count_standardised <- scale(combined_df$tower_count)
#combined_df$mean_distance_from_tower_km_standardised <- scale(combined_df$mean_distance_from_tower_km)
#combined_df$mean_rss_standardised <- scale(combined_df$mean_rss)

### glmm multiple model inference
null_model <- glmmTMB(error_m ~ 1 +
                        (1 | Point_ID),
                      data = combined_df, family = Gamma(link = "log"))

method_only <- glmmTMB(error_m ~ method_name +
                         (1 | Point_ID),
                       data = combined_df, family = Gamma(link = "log"))

properties_only <- glmmTMB(error_m ~ tower_count + Interval_seconds +
                             mean_distance_from_tower_km +
                           mean_rss +
                         (1 | Point_ID),
                       data = combined_df, family = Gamma(link = "log"))

all_no_interac <- glmmTMB(error_m ~ method_name + tower_count + Interval_seconds +
                            mean_distance_from_tower_km + mean_rss +
                            (1 | Point_ID),
                          data = combined_df, family = Gamma(link = "log"))

all_method_interac <- glmmTMB(error_m ~ method_name * (tower_count + Interval_seconds +
                            mean_distance_from_tower_km + mean_rss) +
                            (1 | Point_ID),
                          data = combined_df, family = Gamma(link = "log"))

model.sel(null_model, method_only, properties_only, all_no_interac, all_method_interac, rank = "AICc")

best_model <- all_method_interac

summary(best_model)

# Plot residuals
simulationOutput <- simulateResiduals(fittedModel = best_model, plot = F)

# Create the residual plot and save it using the base R method
#png("paper_results/Figures/glmmm_residuals.png", width = 200, height = 140, units = "mm", res = 300)
plot(simulationOutput)
#dev.off()

tab_model(best_model, show.std = TRUE, show.est = FALSE)

# Number of observations
nrow(combined_df)

# R-squared for GLMM
performance::r2(best_model)

### Plot output
theme_set(theme_bw())
yl = "Positional error (m)"

# Plot effects
odds_plot <- plot_model(best_model, sort.est = TRUE, type = 'std', vline.color = "black", show.p = TRUE, show.values = TRUE, 
           value.offset = 0.4, value.size = 4, title="")

odds_plot <- odds_plot + 
  scale_x_discrete(expand = expansion(add = c(0.5, 0.8)),
                   labels=list(
                     'method_nameAngulation with\nintersect' = 'Angulation with intersect',
                     'method_nameAngulation with\ndistance' = 'Angulation with distance',
                     'tower_count' = 'Tower count',
                     'Interval_seconds13' = 'Interval (13 seconds)',
                     'mean_distance_from_tower_km' = 'Mean distance from tower',
                     'mean_rss' = 'Mean relative signal strength',
                     'method_nameAngulation with\nintersect:tower_count' = 'Angulation with intersect x\ntower count',
                     'method_nameAngulation with\ndistance:tower_count' = 'Angulation with distance x\ntower count',
                     'method_nameAngulation with\nintersect:Interval_seconds13' = 'Angulation with intersect x\ntag interval',
                     'method_nameAngulation with\ndistance:Interval_seconds13' = 'Angulation with distance x\ntag interval',
                     'method_nameAngulation with\nintersect:mean_distance_from_tower_km' = 'Angulation with intersect x\nmean distance from tower',
                     'method_nameAngulation with\ndistance:mean_distance_from_tower_km' = 'Angulation with distance x\nmean distance from tower',
                     'method_nameAngulation with\nintersect:mean_rss' = 'Angulation with intersect x\nmean relative signal strength',
                     'method_nameAngulation with\ndistance:mean_rss' = 'Angulation with distance x\nmean relative signal strength'
                   )) +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1)))

plot(odds_plot)

odds_plot$data # to get variable names

# Plot methods
plot_method <- plot_model(best_model, type = "eff", terms = c("method_name"),show.legend = FALSE,
                    dot.size = 1.5, line.size = 0.8, title = "",
                    axis.title = c("", yl),
                    colors = 'black', show.data = FALSE, dot.alpha = 0.3)+ 
  coord_cartesian(xlim=c(0.7,3.3)) +
  scale_y_continuous(limits = c(200, NA)) +  # Start y-axis at 200
  theme(panel.grid.major.x = element_blank(),  # Remove vertical grid lines
        panel.grid.minor.x = element_blank())

plot(plot_method)

#ggsave("paper_results/Figures/Fig5.jpeg", plot = plot_method, width = 110, height = 100, units = "mm", dpi = 300)

# Plot1
plot1 <- plot_model(best_model, type = "eff", terms = c("mean_distance_from_tower_km"),show.legend = FALSE,
                    dot.size = 1.5, line.size = 0.8, title = "",
                    axis.title = c("Mean distance to receiver (km)", yl), jitter = 0.01,
                    colors = 'Set1', show.data = TRUE, dot.alpha = 0.3) +
  labs(tag = "(A)")
plot(plot1)

# Plot2
plot2 <- plot_model(best_model, type = "eff", terms = c("mean_rss"),show.legend = FALSE,
                    dot.size = 1.5, line.size = 0.8, title = "",
                    axis.title = c("Mean signal strength (RSS)", yl), jitter = 0.01,
                    colors = 'Set1', show.data = TRUE, dot.alpha = 0.3) +
  labs(tag = "(B)")
plot(plot2)

# Plot3
plot3 <- plot_model(best_model, type = "eff", terms = "Interval_seconds",show.legend = FALSE,
                    dot.size = 1.5, line.size = 0.8, title = "",
                    axis.title = c("Pulse interval (seconds)", yl),
                    colors = 'Set1') +
  labs(tag = "(C)")+ 
  coord_cartesian(xlim=c(0,16), ylim=c(0,500))
plot(plot3)

# Plot4
plot4 <- plot_model(best_model, type = "eff", terms = "tower_count",
                    dot.size = 1.5, line.size = 0.8, title = "",
                    axis.title = c("Tower count", yl), jitter = 0.08,
                    colors = 'Set1', ci.lvl = 0.95, show.data = TRUE, dot.alpha = 0.3) +
  labs(tag = "(D)") +
  scale_x_continuous(breaks = seq(1, 9, by = 2))
  
plot(plot4)

# Arrange subplots in a grid layout with labels
plot_grid <- grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)

# Save the plot grid
#ggsave("paper_results/Figures/Fig6.png", plot = plot_grid, width = 160, height = 160, units = "mm", dpi = 300)

# Interaction plots
intplot1 <- plot_model(best_model, type = "eff", 
                    terms = c("Interval_seconds", "method_name"),
                    show.legend = FALSE, dot.size = 1.5, line.size = 0.8, title = "",
                    axis.title = c("Pulse interval (seconds)", yl), jitter = 0.04,
                    colors = 'Set1', show.data = FALSE, dot.alpha = 0.1, dodge = 2) +
  labs(tag = "(A)", color = "Tag interval") +
  theme(legend.position = "bottom",
        plot.tag.position = c(0.03, 0.95)) +
  coord_cartesian(xlim = c(0, 16))
plot(intplot1)

intplot2 <- plot_model(best_model, type = "eff", 
                       terms = c("mean_distance_from_tower_km", "method_name"),
                       show.legend = TRUE, dot.size = 1.5, line.size = 0.8, title = "",
                       axis.title = c("Mean distance to receiver (km)", yl), jitter = 0.04,
                       colors = 'Set1', show.data = FALSE, dot.alpha = 0.1, dodge = .4) +
  labs(tag = "(B)", color = "Method") +
  theme(legend.position = "top",
        plot.tag.position = c(0.03, 0.95))+ 
  guides(color = guide_legend(nrow = 3, byrow = TRUE))
plot(intplot2)

intplot3 <- plot_model(best_model, type = "eff", 
                       terms = c("mean_rss", "method_name"),
                       show.legend = FALSE, dot.size = 1.5, line.size = 0.8, title = "",
                       axis.title = c("Mean signal strength (RSS)", yl), jitter = 0.04,
                       colors = 'Set1', show.data = FALSE, dot.alpha = 0.4, dodge = .4) +
  labs(tag = "(C)", color = "Mean signal strength (RSS)") +
  theme(legend.position = "top",
        plot.tag.position = c(0.03, 0.95))
plot(intplot3)

# Extract the legend from one of the plots (assuming all plots share a compatible legend)
legend <- get_legend(intplot2 + theme(legend.position = "bottom"))

# Remove the legends from the individual plots
intplot1 <- intplot1 + theme(legend.position="none")
intplot2 <- intplot2 + theme(legend.position="none")
intplot3 <- intplot3 + theme(legend.position="none")

# Combine your plots in a single column
combined_plots <- plot_grid(intplot1, intplot2, intplot3, ncol = 1)

# Use cowplot to arrange the legend on top of the combined plots
# Adjust `rel_heights` to control the space allocated to the legend vs. the plots
final_plot <- plot_grid(legend, combined_plots, ncol = 1, rel_heights = c(0.12, 1))

# Display the final plot
print(final_plot)

#ggsave("paper_results/Figures/Fig7.jpeg", plot = final_plot, width = 85, height = 250, units = "mm", dpi = 300)


# Calculate statistics for the paper
# Calculate mean, median, standard deviation, and count for positional errors for each method
error_stats <- combined_df %>%
  group_by(model) %>%
  summarise(
    mean_error = mean(error_m, na.rm = TRUE),
    median_error = median(error_m, na.rm = TRUE),
    sd_error = sd(error_m, na.rm = TRUE),
    count = n()
  )

# Calculate standard error for the mean
error_stats$se_mean = error_stats$sd_error / sqrt(error_stats$count)
