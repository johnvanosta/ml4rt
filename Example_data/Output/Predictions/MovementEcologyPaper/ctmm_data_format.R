
# Script to format data for ingestion into CTMM for BTF home range calcs

# Clear environment
rm(list = ls())

################ Import data ########################
# Load libraries
pacman::p_load(dplyr, readr, openxlsx, readxl, ggplot2, lubridate, tidyr)

# Set working directory
setwd("~/GitHub/ml4rt/Example_data/Output/Predictions/MovementEcologyPaper")

# Import the xlsx file for transect data
art_data <- read_excel("predictions_active_tags_20231209.xlsx")

# Import the xlsx file for the individual locations, excluding ART data
indiv_locs_ex_art <- read_excel("BTF_Individual_Locations_20230718.xlsx")

# Import the xlsx file for the tag list
tag_list <- read_excel("Tag_List.xlsx")

############### Data structuring ######################
# Calculate the average latitude_pred and longitude_pred for each unique combination of DateTime and TagID
average_coordinates <- art_data %>%
  group_by(DateTime, TagID) %>%
  summarise(
    latitude_pred = mean(latitude_pred, na.rm = TRUE),
    longitude_pred = mean(longitude_pred, na.rm = TRUE)
  )

# Merge art_data with tag_list
predictions <- average_coordinates %>% 
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

# Look for times within 5 minutes of another time for that ID and remove the latter
locs4ctmm <- concatenated_df %>%
  # Create a new dataframe with only the specified columns
  select(MetalBand, DateTime, TagID, Method, BTF_Activity_Report, Veracity, POINT_Y, POINT_X, GUIDCOPY) %>%
  # Filter df to include only rows where Method is appropriate for the purpose of calculating home range
  filter(Method %in% c("ART", "Radio tracking", "1km transect", "Repeat observation", "Mist-netting")) %>%
  # Rename columns
  rename(
    ID = MetalBand,
    timestamp = DateTime,
    longitude = POINT_X,
    latitude = POINT_Y
  ) %>%
  # Filter out IDs with less than 5 rows
  group_by(ID) %>%
  # Sort by timestamp within each ID
  arrange(ID, timestamp) %>%
  # Flag rows that are within 5 minutes of the previous row
  mutate(time_diff = timestamp - lag(timestamp),
         keep = ifelse(is.na(time_diff) | time_diff >= minutes(5), TRUE, FALSE)) %>%
  # Keep only the flagged rows
  filter(keep) %>%
  filter(n() >= 5) %>%
  ungroup()

# Add an error column called 'HDOP' that's 209.1 for ART data or 20 for anything else
locs4ctmm <- locs4ctmm %>%
  mutate(UERE = if_else(Method == "ART", 209.1, 20))

write.xlsx(locs4ctmm, "ctmm_input/ctmm_input_20240107_with_lat_long.xlsx")

############## Summary Stats #######################

# Import locs for ctmm after manual screen
locs4ctmm_manual_screen <- read_excel("ctmm_input/ctmm_input_20240107.xlsx")

# Print a count of the number of unique MetalBand values
print(n_distinct(locs4ctmm_manual_screen$ID))
cat("Mean value:", mean(locs4ctmm_manual_screen %>% group_by(ID) %>% summarise(Count = n()) %>% pull(Count)), "\n")
cat("Minimum value:", min(locs4ctmm_manual_screen %>% group_by(ID) %>% summarise(Count = n()) %>% pull(Count)), "\n")
cat("Maximum value:", max(locs4ctmm_manual_screen %>% group_by(ID) %>% summarise(Count = n()) %>% pull(Count)), "\n")
cat("Median value:", median(locs4ctmm_manual_screen %>% group_by(ID) %>% summarise(Count = n()) %>% pull(Count)), "\n")


### Summary of the number of detections (unfiltered per bird)
# Concatenate predictions and indiv_locs_ex_art
concatenated_df_unfiltered <- bind_rows(indiv_locs_ex_art, predictions)

# Create dataframe for all, unfiltered, data
locs4ctmm_all <- concatenated_df_unfiltered %>%
  # Create a new dataframe with only the specified columns
  select(MetalBand, DateTime, TagID, Method, BTF_Activity_Report, Veracity, POINT_Y, POINT_X, GUIDCOPY) %>%
  # Filter df to include only rows where Method is appropriate for the purpose of calculating home range
  filter(Method %in% c("ART", "Radio tracking", "1km transect", "Repeat observation", "Mist-netting")) %>%
  # Filter to include only rows where 'ID' is within the 'ID' field of 'locs4ctmm'
  filter(MetalBand %in% locs4ctmm$ID) %>%
  # Rename columns
  rename(
    ID = MetalBand,
    timestamp = DateTime,
    longitude = POINT_X,
    latitude = POINT_Y
  )

print(n_distinct(locs4ctmm_all$ID))
cat("Mean value:", mean(locs4ctmm_all %>% group_by(ID) %>% summarise(Count = n()) %>% pull(Count)), "\n")
cat("Minimum value:", min(locs4ctmm_all %>% group_by(ID) %>% summarise(Count = n()) %>% pull(Count)), "\n")
cat("Maximum value:", max(locs4ctmm_all %>% group_by(ID) %>% summarise(Count = n()) %>% pull(Count)), "\n")
cat("Median value:", median(locs4ctmm_all %>% group_by(ID) %>% summarise(Count = n()) %>% pull(Count)), "\n")


# Creating a complete list of unique IDs
all_ids <- data.frame(ID = unique(locs4ctmm_all$ID))

# Grouping by Method and ID for "Radio tracking" and "1km transect"
radio_transect_data <- locs4ctmm_all %>%
  filter(Method %in% c("Mist-netting", "Repeat observation", "Radio tracking", "1km transect")) %>%
  group_by(ID) %>%
  summarise(Count = n(), .groups = "drop")

# Join with the complete list of IDs and replace NA with 0
radio_transect_data <- all_ids %>%
  left_join(radio_transect_data, by = "ID") %>%
  replace_na(list(Count = 0)) %>%
  ungroup()

# Calculate summary statistics
radio_transect_stats <- radio_transect_data %>%
  summarise(
    Mean = mean(Count),
    Min = min(Count),
    Max = max(Count),
    Median = median(Count)
  )

print("Stats for Radio tracking and 1km transect:")
print(radio_transect_stats)

# Grouping by Method and ID for "ART" method
art_data <- locs4ctmm_all %>%
  filter(Method == "ART") %>%
  group_by(ID) %>%
  summarise(Count = n(), .groups = "drop")

# Join with the complete list of IDs and replace NA with 0
art_data <- all_ids %>%
  left_join(art_data, by = "ID") %>%
  replace_na(list(Count = 0)) %>%
  ungroup()

# Calculate summary statistics
art_stats <- art_data %>%
  summarise(
    Mean = mean(Count),
    Min = min(Count),
    Max = max(Count),
    Median = median(Count)
  )

print("Stats for ART method:")
print(art_stats)



