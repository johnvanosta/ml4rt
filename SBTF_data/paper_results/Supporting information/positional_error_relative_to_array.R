# Code to compare the performance of each version of the model (listed in the paper)

rm(list = ls())

pacman::p_load(readxl,openxlsx,dplyr,ggplot2, viridis, boot, glmmTMB, DHARMa, sjPlot, gridExtra, broom.mixed, stringr, tidyr,MuMIn,cowplot)

error_dataset <- read_excel("error_estimates_w_position_in_array.xlsx")

# Fit the GLMM model
model <- glmmTMB(error_m ~ position_in_array,
                 family = Gamma(link = "log"),
                 data = error_dataset)

# Summary of the model
summary(model)

# Plot residuals
simulationOutput <- simulateResiduals(fittedModel = model, plot = F)

# Create the residual plot and save it using the base R method
#png("paper_results/Figures/model_residuals_20240517.png", width = 200, height = 140, units = "mm", res = 1200)
plot(simulationOutput)

tab_model(model, show.std = TRUE, show.est = FALSE)

### Plot output
theme_set(theme_bw())
yl = "Positional error (m)"


# Plot methods
plot_location <- plot_model(model, type = "eff", terms = c("position_in_array"),show.legend = FALSE,
                    dot.size = 1.5, line.size = 0.8, title = "",
                    axis.title = c("", yl),
                    colors = 'black', show.data = FALSE, dot.alpha = 0.3)+ 
  coord_cartesian(xlim=c(0.7,2.3)) +
  scale_y_continuous(limits = c(200, NA)) +  # Start y-axis at 200
  theme(panel.grid.major.x = element_blank(),  # Remove vertical grid lines
        panel.grid.minor.x = element_blank())

plot(plot_location)

ggsave("error_vs_position_in_array.jpeg", plot = plot_location, width = 110, height = 100, units = "mm", dpi = 300)

