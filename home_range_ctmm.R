# Script to calculate BTF home ranges

# Clear environment
rm(list = ls())

################ Import data ########################
# Load libraries
pacman::p_load(dplyr, readr, readxl, ggplot2, lubridate, ctmm)

# Import the xlsx file for transect data
art_data <- read_excel("C:/Users/s5236256/Documents/GitHub/ml4rt/Example_data/Output/Predictions/MovementEcologyPaper/predictions_active_tags_20231209.xlsx")

# Import the xlsx file for the individual locations, excluding ART data
indiv_locs_ex_art <- read_excel("C:/Users/s5236256/Documents/GitHub/ml4rt/Example_data/Output/Predictions/MovementEcologyPaper/BTF_Individual_Locations_20230718.xlsx")

# Import the xlsx file for the tag list
tag_list <- read_excel("C:/Users/s5236256/Documents/GitHub/ml4rt/Example_data/Output/Predictions/MovementEcologyPaper/Tag_List.xlsx")

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
  # Filter out IDs with less than 3 rows
  group_by(ID) %>%
  filter(n() >= 3) %>%
  # Sort by timestamp within each ID
  arrange(ID, timestamp) %>%
  # Flag rows that are within 5 minutes of the previous row
  mutate(time_diff = timestamp - lag(timestamp),
         keep = ifelse(is.na(time_diff) | time_diff >= minutes(5), TRUE, FALSE)) %>%
  # Keep only the flagged rows
  filter(keep) %>%
  ungroup()


############### Calculate home ranges in ctmm ######################
# Following Tutorial here: https://ecoisilva.github.io/AKDE_minireview/code/AKDE_R-tutorial.html

# Convert to a telemetry object
locs <- as.telemetry(locs4ctmm)

single_tag <- locs[[3]]

plot(single_tag)

GUESS <- ctmm.guess(single_tag,interactive=FALSE) # automated model guess

# Automated model selection, starting from GUESS:
FIT1_pHREML <- ctmm.select(single_tag, GUESS, method = 'pHREML')

summary(FIT1_pHREML)

UD1w_pHREML <- akde(single_tag, FIT1_pHREML, weights = TRUE) # weighted AKDEc

summary(UD1w_pHREML)$CI # home range area estimation

summary(UD1w_pHREML)$DOF["area"] # effective sample size of animal1

nrow(single_tag) # absolute sample size

BOOT <- ctmm.boot(single_tag, FIT1_pHREML, trace = 2)

UD1w_pHREML_boot <- akde(single_tag, BOOT, weights = TRUE)

summary(UD1w_pHREML_boot)$CI # home range area estimation



# Creating an extent that includes both UDs at the 95% CI level:
EXT <- extent(list(UD1w_pHREML, UD1w_pHREML_boot), level = 0.95)

# Plotting pHREML (with and without weights) side-by-side:
par(mfrow = c(1,2))
plot(single_tag, UD = UD1w_pHREML, ext = EXT)
title(expression("pHREML wAKDE"["C"]))
plot(single_tag, UD = UD1w_pHREML_boot, ext = EXT)
title(expression("pHREML bootstrapped wAKDE"["C"]))




## Example data
data(buffalo)
Pepper <- buffalo$Pepper
