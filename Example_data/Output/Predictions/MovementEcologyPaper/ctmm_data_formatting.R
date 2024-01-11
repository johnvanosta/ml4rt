# Script to calculate BTF home ranges

# Clear environment
rm(list = ls())

################ Import data ########################
# Load libraries
pacman::p_load(dplyr, readr, openxlsx, readxl, ggplot2, lubridate)

# Import the xlsx file for transect data
art_data <- read_excel("C:/Users/JohnvanOsta/Documents/GitHub/ml4rt/Example_data/Output/Predictions/MovementEcologyPaper/predictions_active_tags_20231209.xlsx")

# Import the xlsx file for the individual locations, excluding ART data
indiv_locs_ex_art <- read_excel("C:/Users/JohnvanOsta/Documents/GitHub/ml4rt/Example_data/Output/Predictions/MovementEcologyPaper/BTF_Individual_Locations_20230718.xlsx")

# Import the xlsx file for the tag list
tag_list <- read_excel("C:/Users/JohnvanOsta/Documents/GitHub/ml4rt/Example_data/Output/Predictions/MovementEcologyPaper/Tag_List.xlsx")

############### Data structuring ######################
# Merge art_data with tag_list
predictions <- art_data %>% 
  left_join(tag_list, by = c("TagID" = "Report_Tag_ID"))

# Rename some columns
names(predictions)[names(predictions) == "latitude_pred"] <- "POINT_Y"
names(predictions)[names(predictions) == "longitude_pred"] <- "POINT_X"
names(predictions)[names(predictions) == "Band_Number"] <- "MetalBand"

# Add a new column 'Method' with all values set to 'ART'
predictions$Method <- "ART"

# Assuming indiv_locs is the dataframe for individual locations excluding ART data (loaded earlier)
names(indiv_locs_ex_art)[names(indiv_locs_ex_art) == "CreationDateAEST"] <- "DateTime"
names(indiv_locs_ex_art)[names(indiv_locs_ex_art) == "RadioTag_ID"] <- "TagID"

# Convert 'TagID' in both dataframes to characters
predictions$TagID <- as.character(predictions$TagID)
indiv_locs_ex_art$TagID <- as.character(indiv_locs_ex_art$TagID)

######### Filter ART data to n randomly selected points per day
n_points_per_day <- 2

predictions$Date <- as.Date(predictions$DateTime)

filtered_predictions <- predictions %>%
  group_by(Date, MetalBand) %>%
  sample_n(n_points_per_day, replace = TRUE) %>%
  ungroup()

# Concatenate predictions and indiv_locs_ex_art
concatenated_df <- bind_rows(indiv_locs_ex_art, filtered_predictions)

library(dplyr)
library(lubridate) # for date-time operations

locs4ctmm <- concatenated_df %>%
  # Create a new dataframe with only the specified columns
  select(MetalBand, DateTime, TagID, Method, BTF_Activity_Report, Veracity, POINT_Y, POINT_X, GUIDCOPY) %>%
  # Filter df to include only rows where Method is appropriate
  filter(Method %in% c("ART", "Radio tracking", "1km transect", "Repeat observation", "Mist-netting")) %>%
  # Rename columns
  rename(
    ID = MetalBand,
    timestamp = DateTime,
    longitude = POINT_X,
    latitude = POINT_Y
  ) %>%
  # Sort by ID and timestamp
  arrange(ID, timestamp) %>%
  # Group by ID
  group_by(ID) %>%
  # Calculate the time difference from the previous row
  mutate(time_diff = timestamp - lag(timestamp)) %>%
  # Flag rows that are NOT within 5 minutes of the previous row
  mutate(keep = is.na(time_diff) | time_diff > minutes(5)) %>%
  # Keep only the flagged rows
  filter(keep) %>%
  # Ungroup to remove the effect of the ID grouping
  ungroup() %>%
  # Regroup by ID
  group_by(ID) %>%
  # Filter out IDs with less than 5 rows
  filter(n() >= 5) %>%
  # Ungroup to finalize
  ungroup()


# Add an error column called 'HDOP' that's 209.1 for ART data or 20 for anything else
locs4ctmm <- locs4ctmm %>%
  mutate(UERE = if_else(Method == "ART", 209.1, 20))

write.xlsx(locs4ctmm, file="C:/Users/JohnvanOsta/Documents/GitHub/ml4rt/Example_data/Output/Predictions/MovementEcologyPaper/ctmm_input/ctmm_input_20231219.xlsx")

# Then I manually screened to remove mist-net records outside of the tracking period