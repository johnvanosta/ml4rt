pacman::p_load(ggplot2,MuMIn,lme4,glmmTMB,performance,sjPlot,
               DHARMa,dplyr,readxl,gridExtra, GGally, logistf, broom, tibble)

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
  mutate(
    RE = case_when(
      RE_Code != "Non-remnant" ~ sapply(strsplit(gsub("[a-zA-Z]", "", RE_Code), "/"), `[`, 1),
      TRUE ~ RE_Code
    ),
    RE = case_when(
      RE %in% c("10.3.6", "10.3.61", "11.3.10") ~ "E. brownii W.",
      RE == "10.5.5" ~ "E. melanophloia W.",
      RE %in% c("10.3.15", "11.3.3", "11.3.25", "11.3.27", "10.3.13", "10.3.14") ~ "E. camaldulensis W.",
      RE %in% c("10.10.1", "10.7.3") ~ "A. shirleyi O.F.",
      RE %in% c("10.3.4", "10.4.5","11.3.5", "11.4.6") ~ "A. cambagei W.",
      RE %in% c("11.4.9", "10.4.3") ~ "A. harpophylla W.",
      RE %in% c("10.7.4", "10.7.5") ~ "E. persistens W.",
      RE %in% c("10.3.12", "10.5.2") ~ "Corymbia spp. W.",
      RE == "10.5.1" ~ "E. similis W.",
      RE == "10.7.7" ~ "M. tamariscina W.",
      TRUE ~ RE
    ),
    PermanentDistance_km = PermanentDistance / 1000,
    TrackDistance_km = TrackDistance / 1000,
    EphemeralDistance_km = EphemeralDistance / 1000
  )

# Trial to remove ART records
#filt_data <- filt_data %>%
  #filter(Method != "ART")

unique(filt_data$RE)

RE_Code_count <- count(filt_data, RE)

print(RE_Code_count, n = 200)

### Data exploration
hist(filt_data$BTF_presence, main = "BTF presence", 
     xlab = "BTF_presence", border = "blue", col = "lightblue")

#ggpairs(filt_data, 
        #columns = c("BTF_presence", "EphemeralDistance", "PermanentDistance", "TrackDistance", "RE"),
        #title = "Pairwise Relationships")

# Create a contingency table for RE
contingency_table <- table(filt_data$RE, filt_data$BTF_presence)
print(contingency_table)


###### Descriptive statistics for the report
## Mean, min, max and median, distance to permanent water
# Get prseence data
pres_data <- subset(filt_data, Method != "Random")

# Calc summary stats
mean(pres_data$PermanentDistance, na.rm = TRUE)
min(pres_data$PermanentDistance, na.rm = TRUE)
max(pres_data$PermanentDistance, na.rm = TRUE)
median(pres_data$PermanentDistance, na.rm = TRUE)

### model selection
# Note: after trying a glm, there was an issue with model convergence due to separation of variance among RE's.
# Model has been updated to use a PENALIZED MAXIMUM LIKELIHOOD ESTIMATION method (Firth's)

m1 <- logistf(BTF_presence ~ RE + PermanentDistance_km + TrackDistance_km, data = filt_data)

summary(m1)

m2 <- logistf(BTF_presence ~ RE + PermanentDistance_km + TrackDistance_km + EphemeralDistance_km + Season, data = filt_data)

summary(m2)

m3 <- logistf(BTF_presence ~ RE + PermanentDistance_km + TrackDistance_km + Season:PermanentDistance_km + Season:RE, data = filt_data)

summary(m3)

best_model <- m3

# Extract model estimates and write to a text file
model_summary <- capture.output(summary(best_model))
#writeLines(model_summary, "figures/habitat_selection_model_20240111.txt")

####### Plot the results
### Set theme for the plots, theme_bw is a good base one for scientific papers
theme_set(theme_bw())

odds_plot <- plot_model(best_model, sort.est = TRUE, vline.color = "black", show.p = TRUE, show.values = TRUE, 
            value.offset = 0.4, value.size = 4, title="")

odds_plot <- odds_plot + 
  scale_x_discrete(expand = expansion(add = c(0.5, 0.8)),
                   labels=list(
                     'REA. harpophylla W.' = 'A. harpophylla W.',
                     'REA. shirleyi O.F.' = 'A. shirleyi O.F.',
                     'RECorymbia spp. W.' = 'Corymbia spp. W.',
                     'REE. brownii W.' = 'E. brownii W.',
                     'REE. camaldulensis W.' = 'E. camaldulensis W.',
                     'REE. melanophloia W.' = 'E. melanophloia W.',
                     'REE. persistens W.' = 'E. persistens W.',
                     'REE. similis W.' = 'E. similis W.',
                     'REM. tamariscina W.' = 'M. tamariscina W.',
                     'RENon-remnant' = 'Non-remnant',
                     'PermanentDistance_km' = 'Permanent Distance (km)',
                     'TrackDistance_km' = 'Track Distance (km)',
                     'PermanentDistance_km:SeasonWet' = 'Permanent Distance (km) x Wet Season',
                     'REA. harpophylla W.:SeasonWet' = 'A. harpophylla W. x Wet Season',
                     'REA. shirleyi O.F.:SeasonWet' = 'A. shirleyi O.F. x Wet Season',
                     'RECorymbia spp. W.:SeasonWet' = 'Corymbia spp. W. x Wet Season',
                     'REE. brownii W.:SeasonWet' = 'E. brownii W. x Wet Season',
                     'REE. camaldulensis W.:SeasonWet' = 'E. camaldulensis W. x Wet Season',
                     'REE. melanophloia W.:SeasonWet' = 'E. melanophloia W. x Wet Season',
                     'REE. persistens W.:SeasonWet' = 'E. persistens W. x Wet Season',
                     'REE. similis W.:SeasonWet' = 'E. similis W. x Wet Season',
                     'REM. tamariscina W.:SeasonWet' = 'M. tamariscina W. x Wet Season',
                     'RENon-remnant:SeasonWet' = 'Non-remnant x Wet Season'
                   ))
                   
plot(odds_plot)

# Save the plot
ggsave("figures/odds_plot_20240119.png", plot = odds_plot, unit = 'cm', width = 16, height = 18, dpi = 300)

# Plots of different components of the model
plot_RE <- plot_model(best_model,
                   type = "eff",
                   terms = c("RE"),
                   show.legend = FALSE,
                   title = ""         
) + coord_flip()

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







# Combine the plots into a grid
combined_plot <- grid.arrange(plot_shrub_cover, plot_grass_cover, plot_canopy_height, ncol = 1)

# Save the combined plot
ggsave("X:/JOBS/~2019/QEJ19152/DATA ANALYSIS/HABITAT/NEST_SELECTION/Output/combined_plots_20231214.png", 
       plot = combined_plot, 
       width = 10, height = 20, units = "cm", dpi = 300)

