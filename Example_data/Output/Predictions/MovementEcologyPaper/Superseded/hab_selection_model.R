# Script to calculate BTF home ranges

# Clear environment
rm(list = ls())

################ Import data ########################
# Load libraries
pacman::p_load(dplyr, readr, openxlsx, readxl, ggplot2, amt)

# Set working directory
setwd("~/GitHub/ml4rt/Example_data/Output/Predictions/MovementEcologyPaper")

# Import the xlsx file for location data
locs4amt <- read_excel("ctmm_input/ctmm_input_20231222.xlsx")

# Rename columns
locs4amt <- locs4amt %>%
  rename(y_ = latitude, x_ = longitude, t_ = timestamp)

single_tag <- locs4amt %>% filter(ID == "024-32012")

### Following vignettes in the amt package: https://cran.r-project.org/web/packages/amt/index.html
# Make a track # could also do this using time

# Initialize an empty dataframe to store results
all_rpoints <- data.frame()




# Create random points separately for tags

# Initialize an empty dataframe to store results
all_rpoints <- data.frame()

# Loop through unique IDs in locs4amt
for (i in unique(locs4amt$ID)) {
  
  # 1. Filter locs4amt for that tag
  single_ID <- locs4amt %>% filter(ID == i)
  
  # 2. Create a track and random points for that ID
  trk <- make_track(single_ID, x_, y_, id = ID)
  
  hr <- hr_mcp(trk)|> hr_isopleths(levels=1) |> 
    sf::st_buffer(dist = 0.018) # Distance in degrees, so 0.009 for each 1 km
  
  rpoints <- random_points(hr, presence = trk)
  
  # 3. Append the ID to the rpoints dataframe
  rpoints$ID <- i  # Add a new column with the ID
  
  # Combine with the overall results
  all_rpoints <- rbind(all_rpoints, rpoints)
}

# Create the map plot
ggplot(all_rpoints, aes(x = x_, y = y_, color = case_)) +
  geom_point() +
  labs(x = "Longitude", y = "Latitude", color = "Case") +
  theme_minimal() +
  ggtitle("Spatial Locations Map")





tr1 <- make_track(locs4amt, x_, y_)

r1 <- random_points(tr1)


# Create the map plot
ggplot(r1, aes(x = x_, y = y_, color = case_)) +
  geom_point() +
  labs(x = "Longitude", y = "Latitude", color = "Case") +
  theme_minimal() +
  ggtitle("Spatial Locations Map")

