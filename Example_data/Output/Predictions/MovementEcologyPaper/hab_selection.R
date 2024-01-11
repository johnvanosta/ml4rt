pacman::p_load(ggplot2,MuMIn,lme4,glmmTMB,performance,sjPlot,
               DHARMa,dplyr,readxl,gridExtra, GGally, logistf, broom)

rm(list = ls()) # Cleans the working environment

# Set working directory
setwd("~/GitHub/ml4rt/Example_data/Output/Predictions/MovementEcologyPaper")

hab_sel_data <- read_excel('GIS_Output/Hab_selection_locs_w_random_20240110.xlsx')

###### Data filtering steps
# Selecting specific columns
filt_data <- hab_sel_data %>%
  select(Type, BTF_presence, Season, Method, EphemeralDistance, PermanentDistance, TrackDistance, RE_Code)%>%
  filter(RE_Code != "<Null>") %>%
  filter(Method != "Mist-netting")

# Adjusting RE_Code
filt_data <- filt_data %>%
  mutate(RE_Code_filt = ifelse(RE_Code != "Non-remnant", 
                               sapply(strsplit(gsub("[a-zA-Z]", "", RE_Code), "/"), `[`, 1),
                               RE_Code)) %>%
  mutate(RE_Code_filt = ifelse(RE_Code_filt %in% c("10.3.6", "10.3.61", "11.3.10"), 
                               "10.3.6/10.5.5x2/11.3.10", RE_Code_filt)) %>%
  mutate(RE_Code_filt = ifelse(RE_Code_filt == "10.5.5", "10.5.5a", RE_Code_filt)) %>%
  mutate(RE_Code_filt = ifelse(RE_Code_filt %in% c("10.3.15", "11.3.3", "11.3.25", "11.3.27", "10.3.13", "10.3.14"), 
                               "10.3.15/10.3.13/10.3.14/11.3.3/11.3.25/11.3.27", RE_Code_filt)) %>%
  mutate(RE_Code_filt = ifelse(RE_Code_filt %in% c("10.10.1", "10.7.3"), 
                               "10.10.1/10.7.3", RE_Code_filt)) %>%
  mutate(RE_Code_filt = ifelse(RE_Code_filt %in% c("10.3.4", "10.4.5","11.3.5", "11.4.6"), 
                               "10.3.4/10.4.5/11.3.5/11.4.6", RE_Code_filt)) %>%
  mutate(RE_Code_filt = ifelse(RE_Code_filt %in% c("11.4.9", "10.4.3"), 
                               "10.4.3/11.4.9", RE_Code_filt)) %>%
  mutate(RE_Code_filt = ifelse(RE_Code_filt %in% c("10.7.4", "10.7.5"), 
                               "10.7.4/10.7.5", RE_Code_filt)) %>%
  mutate(RE_Code_filt = ifelse(RE_Code_filt %in% c("10.3.12", "10.5.2"), 
                               "10.3.12/10.5.2", RE_Code_filt)) %>%
  mutate(PermanentDistance_km = PermanentDistance/1000) %>%
  mutate(TrackDistance_km = TrackDistance/1000) %>%
  mutate(EphemeralDistance_km = EphemeralDistance/1000)

# Trial to remove ART records
#filt_data <- filt_data %>%
  #filter(Method != "ART")

unique(filt_data$RE_Code_filt)

RE_Code_count <- count(filt_data, RE_Code_filt)

print(RE_Code_count, n = 200)

### Data exploration
hist(filt_data$BTF_presence, main = "BTF presence", 
     xlab = "BTF_presence", border = "blue", col = "lightblue")

#ggpairs(filt_data, 
        #columns = c("BTF_presence", "EphemeralDistance", "PermanentDistance", "TrackDistance", "RE_Code_filt"),
        #title = "Pairwise Relationships")

# Create a contingency table for RE
contingency_table <- table(filt_data$RE_Code_filt, filt_data$BTF_presence)
print(contingency_table)

### model selection
# Note: after trying a glm, there was an issue with model convergence due to separation of variance among RE's.
# Model has been updated to use a PENALIZED MAXIMUM LIKELIHOOD ESTIMATION method (Firth's)

m1 <- logistf(BTF_presence ~ RE_Code_filt + PermanentDistance_km + TrackDistance_km, data = filt_data)

summary(m1)

m2 <- logistf(BTF_presence ~ RE_Code_filt + PermanentDistance_km + TrackDistance_km + EphemeralDistance_km + Season, data = filt_data)

summary(m2)

m3 <- logistf(BTF_presence ~ RE_Code_filt + PermanentDistance_km + TrackDistance_km + Season:PermanentDistance_km + Season:RE_Code_filt, data = filt_data)

summary(m3)

# Capture the summary of the averaged model and write to a text file

# Extract model estimates
model_summary <- capture.output(summary(m1))
#writeLines(model_summary, "figures/habitat_selection_model_20240111.txt")

best_model <- m3

####### Plot the results
### Set theme for the plots, theme_bw is a good base one for scientific papers
theme_set(theme_bw())

plot_model(best_model, sort.est = TRUE, vline.color = "black")

plot_RE <- plot_model(best_model,
                   type = "eff",
                   terms = c("RE_Code_filt"),
                   show.legend = FALSE,
                   title = ""         
)

plot(plot_RE)

plot_water_dist <- plot_model(best_model,
                      type = "eff",
                      terms = "PermanentDistance_km [all]",
                      show.legend = FALSE,
                      title = ""         
)

plot(plot_water_dist)

plot_track_dist <- plot_model(best_model,
                              type = "eff",
                              terms = "TrackDistance_km [all]",
                              show.legend = FALSE,
                              title = ""
)

plot(plot_track_dist)





# Define a custom function to determine if the 95% CI overlaps with zero
is_significant <- function(model, var) {
  ci <- confint(model, var)
  return(ci[1] > 0 | ci[2] < 0)
}

# Get model estimates and their confidence intervals
estimates <- summary(best_model)$coefficients
variables <- rownames(estimates)

# Determine which variables are significant
significant_vars <- sapply(variables, function(var) is_significant(best_model, var))

# Create a customized plot
plot_model(best_model, sort.est = TRUE, vline.color = "black", 
           geom.colors = ifelse(significant_vars, "black", "grey"), 
           geom.size = ifelse(significant_vars, 1.5, 0.5))






###### Grass cover
plot_grass_cover <- plot_model(best_model,
                               type = "eff",
                               terms = c("Grass_cover"),
                               show.legend = FALSE
)

yplot_grass_cover <- plot_grass_cover +
  labs(title = NULL) +
  ylab("Nest presence") +
  xlab("Grass cover (%)") +
  labs(tag = "(B)")

plot(plot_grass_cover)

##### Canopy height
plot_canopy_height <- plot_model(best_model,
                               type = "eff",
                               terms = c("Canopy_height"),
                               show.legend = FALSE
)

plot_canopy_height <- plot_canopy_height +
  labs(title = NULL) +
  ylab("Nest presence") +
  xlab("Canopy height (%)") +
  labs(tag = "(C)")

plot(plot_canopy_height)

# Combine the plots into a grid
combined_plot <- grid.arrange(plot_shrub_cover, plot_grass_cover, plot_canopy_height, ncol = 1)

# Save the combined plot
ggsave("X:/JOBS/~2019/QEJ19152/DATA ANALYSIS/HABITAT/NEST_SELECTION/Output/combined_plots_20231214.png", 
       plot = combined_plot, 
       width = 10, height = 20, units = "cm", dpi = 300)

