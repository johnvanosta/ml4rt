# Script to analyse BTF diel activity patterns

# Clear environment
rm(list = ls())

# Load libraries
pacman::p_load(dplyr, openxlsx, readxl, ggplot2, activity)

# Import the xlsx file for the individual locations, excluding ART data
indiv_locs_ex_art <- read_excel("C:/Users/s5236256/Documents/GitHub/ml4rt/Example_data/Output/Predictions/MovementEcologyPaper/BTF_Individual_Locations_20230718.xlsx")

#Filter dataframe to include only records where the activity status was known
activity_records <- indiv_locs_ex_art %>%
  filter(BTF_Activity_Report %in% c("Drinking", "Flying/Perching/Preening", "Foraging", "Nesting"))%>%
  # Rename columns
  rename(
    ID = MetalBand,
    timestamp = CreationDateAEST,
    longitude = POINT_X,
    latitude = POINT_Y,
    activity = BTF_Activity_Report
  )

# Following tutorial: https://bookdown.org/c_w_beirne/wildCo-Data-Analysis/activity.html
# use the average ancoring method as recommended by Vazquez

# calculate solar time 
tmp <- solartime ( activity_records$timestamp, # the date time column 
                   activity_records$latitude,  # Latitude
                   activity_records$longitude, # Longitude
                   tz=10,              # an offset in numeric hours to UTC - Check if this is needed
                   format="%Y-%m-%d %H:%M:%S")

# Comparing solar vs clock time
activity_records$solar <- tmp$solar
activity_records$clock <- tmp$clock

plot(activity_records$solar, activity_records$clock)

# Fit an activity model 
m1 <- fitact(activity_records$solar, sample="model", reps=100) # change to 1000 after testing
plot(m1)
m1

# could fit a seperate model for each activity and compare the coefficient of overlap?

# Comparing coefficient of overlap against models. 0 = no overlap, 1 = high overlap. Get's a p value as well
compareCkern(m1, m2, reps = 100)

### Also compare via season
# First pull out the month
activity_records$month <- month(activity_records$timestamp, label=T)

#Fit an activity model for each season
m_early_wet_forage <- fitact(activity_records$solar[activity_records$activity=="Foraging" &
                                               activity_records$month %in% c("Dec", "Jan", "Feb")], sample="model", reps=100)

m_early_wet_nest <- fitact(activity_records$solar[activity_records$activity=="Nesting" &
                                                      activity_records$month %in% c("Dec", "Jan", "Feb")], sample="model", reps=100)

# The compareCkern can only accept two models - maybe a wet vs dry season?
compareCkern(m_early_wet_forage, m_early_wet_nest, reps = 100)