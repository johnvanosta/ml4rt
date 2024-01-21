# BTF dispersal calculations and plot for short-term movement paper

# Clear environment
rm(list = ls())

# Load libraries
pacman::p_load(dplyr, readxl, ggplot2, lubridate, geosphere, ggridges)

# Data Import
RT_data <- read_excel('ctmm_input/ctmm_input_20240107.xlsx')

# Calculate Tag Capture Locations
tag_capture_locs <- RT_data %>%
  filter(Method == "Mist-netting") %>%
  group_by(ID) %>%
  summarize(
    capture_dt = min(timestamp),
    capture_x = min(longitude),
    capture_y = min(latitude)
  )

# Joining tag_capture_locs with RT_data to make capture_x and capture_y available
RT_data <- RT_data %>%
  left_join(tag_capture_locs, by = "ID")

# Calculate Distances Using geosphere
RT_data <- RT_data %>%
  rowwise() %>%
  mutate(
    DistFromCapture = distHaversine(
      c(capture_x, capture_y),
      c(longitude, latitude)
    ), # Units in meters
    DaysAfterCapture = as.numeric(difftime(timestamp, capture_dt, units = "days")),
    DaysAfterCapture = ifelse(DaysAfterCapture < 0, 0, DaysAfterCapture)
  )

# Calculating basic statistics for DistFromCapture
mean_distance <- mean(RT_data$DistFromCapture, na.rm = TRUE)
min_distance <- min(RT_data$DistFromCapture, na.rm = TRUE)
max_distance <- max(RT_data$DistFromCapture, na.rm = TRUE)
std_dev_distance <- sd(RT_data$DistFromCapture, na.rm = TRUE)
std_err_distance <- std_dev_distance / sqrt(sum(!is.na(RT_data$DistFromCapture)))

# Printing the values
print(paste("Mean Distance:", mean_distance))
print(paste("Minimum Distance:", min_distance))
print(paste("Maximum Distance:", max_distance))
print(paste("Standard Deviation of Distance:", std_dev_distance))
print(paste("Standard Error of Distance:", std_err_distance))

# Create a density plot of distance from capture
ggplot(RT_data, aes(x = DistFromCapture)) +
  geom_density(fill = "lightgrey", alpha = 0.7) +
  labs(x = "Distance from Capture (m)", y = "Density") +
  theme_minimal()

# Create a histogram of distance from capture
hist_dist_capture <- ggplot(RT_data, aes(x = DistFromCapture)) +
  geom_histogram(binwidth = 100, alpha = 0.7) +
  labs(x = "Distance from capture (m)", y = "Number of resightings") +
  theme_bw()

#ggsave("figures/DistFromCapture_hist_20240121.png", 
       plot = hist_dist_capture, 
       width = 10, height = 10, units = "cm", dpi = 300)

# Proportion Calculations
prop_less_4km <- sum(RT_data$DistFromCapture < 4000) / nrow(RT_data)

# Calculating Standard Error
n <- nrow(RT_data)  # Total number of observations
std_err <- sqrt((prop_less_4km * (1 - prop_less_4km)) / n)

# Printing the results
print(paste("Proportion of sightings less than 4km:", prop_less_4km))
print(paste("Standard Error of the proportion:", std_err))

# Proportion Calculations
prop_less_1km <- sum(RT_data$DistFromCapture < 1000) / nrow(RT_data)

# Calculating Standard Error
n <- nrow(RT_data)  # Total number of observations
std_err <- sqrt((prop_less_1km * (1 - prop_less_1km)) / n)

# Printing the results
print(paste("Proportion of sightings less than 1km:", prop_less_1km))
print(paste("Standard Error of the proportion:", std_err))




# Fit the Gaussian GLMM
model <- glmmTMB(DistFromCapture ~ DaysAfterCapture + (1 | ID), data = RT_data, family = gaussian())

summary(model)

# Plotting the model
plot_model(model,
                              type = "eff",
                              terms = "DaysAfterCapture",
                              show.legend = FALSE,
                              title = ""         
)



