# Script to calculate BTF home ranges

# Clear environment
rm(list = ls())

################ Import data ########################
# Load libraries
pacman::p_load(dplyr, readr, openxlsx, readxl, ggplot2, lubridate, ctmm, data.table)

base_dir <- "C:/Users/John/Documents/GitHub/ml4rt/Example_data/Output/Predictions/MovementEcologyPaper/ctmm_output"

# Import the xlsx file for transect data
art_data <- read_excel("C:/Users/John/Documents/GitHub/ml4rt/Example_data/Output/Predictions/MovementEcologyPaper/predictions_active_tags_20231209.xlsx")

# Import the xlsx file for the individual locations, excluding ART data
indiv_locs_ex_art <- read_excel("C:/Users/John/Documents/GitHub/ml4rt/Example_data/Output/Predictions/MovementEcologyPaper/BTF_Individual_Locations_20230718.xlsx")

# Import the xlsx file for the tag list
tag_list <- read_excel("C:/Users/John/Documents/GitHub/ml4rt/Example_data/Output/Predictions/MovementEcologyPaper/Tag_List.xlsx")

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

# Add an error column called 'HDOP' that's 209.1 for ART data or 20 for anything else
locs4ctmm <- locs4ctmm %>%
  mutate(UERE = if_else(Method == "ART", 209.1, 20))

############## for testing!!!
# Extract the first two unique IDs and create a subset of locs4ctmm with only the first two unique IDs
#unique_ids <- unique(locs4ctmm$ID)[1:2]
#locs4ctmm <- filter(locs4ctmm, ID %in% unique_ids)

############### Calculate home ranges in ctmm ######################
# Following tutorial here (for error estimation): https://ctmm-initiative.github.io/ctmm/articles/error.html
# and for AKDE method: https://ecoisilva.github.io/AKDE_minireview/code/AKDE_R-tutorial.html

# Set variables for the loop
locs <- as.telemetry(locs4ctmm)

# Load existing home_range_data if it exists
#if(file.exists(file.path(base_dir, "home_range_data.xlsx"))) {
  #home_range_data <- read_xlsx(file.path(base_dir, "home_range_data.xlsx"))
#} else {
  #home_range_data <- data.frame(Name = character(), Rows = integer(), FitSuccess = logical(), stringsAsFactors = FALSE)
#}

# Load existing FITS if it exists
#if(file.exists(file.path(base_dir, "FITS.rds"))) {
  #FITS <- readRDS(file.path(base_dir, "FITS.rds"))
#} else {
  #FITS <- list()
#}

GUESS <- list()
FITS <- list()
home_range_data <- data.frame(Name = character(), Rows = integer(), FitSuccess = logical(), stringsAsFactors = FALSE)

for(i in 1:length(locs)) {
  current_name <- names(locs[i])
  print(current_name)
  # Skip if Name is already in home_range_data
  print(nrow(locs[[i]]))
  tryCatch({
    # Run the bootstrapping step only if nrow is 5 or less
      GUESS[[i]] <- ctmm.guess(locs[[i]], CTMM=ctmm(error=TRUE), interactive = FALSE)
      FITS[[i]] <- ctmm.select(locs[[i]], GUESS[[i]], method = 'pHREML')
      
      # Record success
      home_range_data <- rbind(home_range_data, data.frame(Name = current_name, Rows = nrow(locs[[i]]), FitSuccess = TRUE))
  }, error = function(e) {
      # Record failure
      home_range_data <- rbind(home_range_data, data.frame(Name = current_name, Rows = nrow(locs[[i]]), FitSuccess = FALSE))
  })
    
  # Save FITS after each iteration
  saveRDS(FITS, file = file.path(base_dir, "FITS.rds"))
    
  # Save home_range_data as Excel after each iteration
  write.xlsx(home_range_data, file.path(base_dir, "home_range_data.xlsx"))
  }
}
# calculate AKDES on a consistent grid
AKDES <- list()
AKDES <- akde(locs,FITS,weights=TRUE)

saveRDS(AKDES, file = file.path(base_dir, "AKDES.rds"))

nrow(FITS)

#i = 11
#print(nrow(locs[[i]]))

#single_tag <- locs[[i]]

#GUESS <- ctmm.guess(single_tag, interactive = FALSE)
#FITS <- ctmm.select(single_tag, GUESS, method = 'pHREML')
#BOOTS <- ctmm.boot(single_tag, FITS, iterate = TRUE, trace = TRUE, parallel=FALSE)
    
#require(parallel)
#cores <- detectCores() 
    

### Try population level analyses
# https://besjournals.onlinelibrary.wiley.com/doi/epdf/10.1111/2041-210X.13815
# https://streaming.uni-konstanz.de/paella/?tx_uknkimstreams_player%5Baction%5D=player&tx_uknkimstreams_player%5Bcode%5D=AniMove_2022-09-16_04&tx_uknkimstreams_player%5Bcontroller%5D=StreamList&tx_uknkimstreams_player%5Btitle%5D=&cHash=8a5b5a07f6049d52ad3c38fcf4f047a5

FITS <- readRDS(file.path(base_dir, "FITS.rds"))
AKDES <- readRDS(file.path(base_dir, "AKDES.rds"))

# Filter out some AKDES and locs
is_not_ctmm <- sapply(AKDES, function(x) class(x) != "ctmm")

# Filter out elements of class 'ctmm'
AKDES_filtered <- AKDES[is_not_ctmm]
locs_filtered <- locs[is_not_ctmm]



## Check locations on index 88 and possibly remove



AREAS <- lapply(AKDES_filtered,summary)

AREAS[[1]]$CI

for(i in 1:length(AREAS)){
  print(i)
  print(AREAS[[i]]$CI)
}

for(i in 1:length(AKDES)){
  print(i)
  sample_size <- nrow(locs[[i]])
  ci <- summary(AKDES[[i]])$CI
}



i=12
summary(AKDES[[i]])$CI
summary(AKDES[[i]])$CI[1, "est"]



# Remove troublesome AKDES from the AKDES list
AKDES[[16]] <- NULL
AKDES[[40]] <- NULL
AKDES[[79]] <- NULL
AKDES[[81]] <- NULL
AKDES[[110]] <- NULL
AKDES[[174]] <- NULL



# Identify elements of class 'ctmm'
is_not_ctmm <- sapply(AKDES, function(x) class(x) != "ctmm")

# Filter out elements of class 'ctmm'
AKDES_filtered <- AKDES[is_not_ctmm]
locs_filtered <- locs[is_not_ctmm]

COL <- color(AKDES_filtered, by = 'individual')

plot(AKDES_filtered, col.DF = COL, col.level = COL, col.grid = NA, level = NA)

### Need to make sure that ctmm is the latest version
# cluster analysis
cluster(AKDES, sort = TRUE)

# Mean home ranges, sorted largest to smallest individual
meta(AKDES_filtered, col = c(COL, "black"), verbose = TRUE, sort = FALSE)

# funnel plot to check for sampling bias
funnel(AKDES_filtered,locs_filtered, level=0.5)

overlap(AKDES)

# Compare groups
### Example code, but adjust for season and maybe sex
meta(list(south = AKDES[1:3],
          north = AKDES[4:6]),
     plot = TRUE,
     verbose = TRUE)

## Population models/range (all of the tags together)
MEAN.FITS <- mean(FITS)
summary(MEAN.FITS)

## Mean population range
MEAN <- mean(AKDES_filtered)
plot(locs_filtered, MEAN)

## Better (and newer) function is to use the population KDE function
PKDE <- pkde(locs_filtered, AKDES_filtered)

summary(PKDE)$CI

## See if this can be exported as a shapefile too



# extract 95% areas
AREAS <- lapply(AKDES,summary)

# log transform for further meta-analysis
LOG <- Log(AREAS)



### Export home ranges



# Initialize an empty data frame for home range data
home_range_data <- data.frame()

# Fit movement models
BOOTS <- FITS <- AKDES <- list()
GUESS <- list()
for(i in 1:length(locs)) {
  tryCatch({
    print(i)
    # Attempt to fit the model
    GUESS[[i]] <- ctmm.guess(locs[[i]], CTMM=ctmm(error=TRUE), interactive = FALSE)
    FITS[[i]] <- ctmm.select(locs[[i]], GUESS, method = 'pHREML')
    #BOOTS[[i]] <- ctmm.boot(locs[[i]], FITS[[i]], trace = 2)
    
    # Record success
    home_range_data <- rbind(home_range_data, data.frame(Name = names(locs[i]), Rows = nrow(locs[[i]]), FitSuccess = TRUE))
  }, error = function(e) {
    # Record failure
    home_range_data <- rbind(home_range_data, data.frame(Name = names(locs[i]), Rows = nrow(locs[[i]]), FitSuccess = FALSE))
  })
}
# calculate AKDES on a consistent grid
AKDES <- akde(locs,FITS,weights=TRUE)

saveRDS(AKDES, file = file.path(base_dir, "AKDES.rds"))

names(locs[i])

## Restore from a saved RDS file
#AKDES <- readRDS(file.path(base_dir, "AKDES.rds"))









# extract 95% areas
AREAS <- lapply(test_list,summary(level=0.95,level.UD=0.95))

for(i in 1:length(test_list))
{
  print(summary(test_list[[i]], level=0.95,level.UD=0.95))
}

i = 1
summary(AKDES[1])

# Loop through each ID
for (i in 1:num_ids) {
  id <- names(locs)[i]
  single_tag <- locs[[i]]

  #GUESS <- ctmm.guess(single_tag,CTMM=ctmm(error=TRUE),interactive=FALSE) # automated model guess
  GUESS <- ctmm.guess(single_tag,interactive=FALSE) # model without error for testing

  FIT1_pHREML <- ctmm.select(single_tag, GUESS, method = 'pHREML')

  BOOT <- ctmm.boot(single_tag, FIT1_pHREML, trace = 2)

  UD1w_pHREML_boot <- akde(single_tag, BOOT, weights=TRUE, grid=list(dr=10, align.to.origin=T))
  
  # Extract information
  sample_size <- nrow(single_tag)
  
  ## For 95% home range area
  print(summary(UD1w_pHREML_boot, level=0.95,level.UD=0.95))
  area_km2_0.95_min <- summary(UD1w_pHREML_boot, level=0.95,level.UD=0.95)$CI[1]
  area_km2_0.95_mean <- summary(UD1w_pHREML_boot, level=0.95,level.UD=0.95)$CI[2]
  area_km2_0.95_max <- summary(UD1w_pHREML_boot, level=0.95,level.UD=0.95)$CI[3]
  dof_area_0.95 <- summary(UD1w_pHREML_boot, level=0.95,level.UD=0.95)$DOF["area"]
  dof_bandwidth_0.95 <- summary(UD1w_pHREML_boot, level=0.95,level.UD=0.95)$DOF["bandwidth"]
  
  ## For 50% home range area
  print(summary(UD1w_pHREML_boot, level=0.95,level.UD=0.5))
  area_km2_0.5_min <- summary(UD1w_pHREML_boot, level=0.95,level.UD=0.5)$CI[1]
  area_km2_0.5_mean <- summary(UD1w_pHREML_boot, level=0.95,level.UD=0.5)$CI[2]
  area_km2_0.5_max <- summary(UD1w_pHREML_boot, level=0.95,level.UD=0.5)$CI[3]
  dof_area_0.5 <- summary(UD1w_pHREML_boot, level=0.95,level.UD=0.5)$DOF["area"]
  dof_bandwidth_0.5 <- summary(UD1w_pHREML_boot, level=0.95,level.UD=0.5)$DOF["bandwidth"]

  # For each ID, calculate and append home range data to the data frame
  home_range_data <- rbind(home_range_data, data.frame(
    id = id,
    sample_size = sample_size,
    area_km2_0_95_min = area_km2_0.95_min,
    area_km2_0_95_mean = area_km2_0.95_mean,
    area_km2_0_95_max = area_km2_0.95_max,
    dof_area_0_95 = dof_area_0.95,
    dof_bandwidth_0_95 = dof_bandwidth_0.95,
    area_km2_0_5_min = area_km2_0.5_min,
    area_km2_0_5_mean = area_km2_0.5_mean,
    area_km2_0_5_max = area_km2_0.5_max,
    dof_area_0_5 = dof_area_0.5,
    dof_bandwidth_0_5 = dof_bandwidth_0.5
  ))

  # Export 95% and 50% home ranges and points for the current ID
  # Write UD 95 and UD 50 to ESRI shapefile
  filename_ud95 <- file.path(base_dir, paste0(id, "_UD_95"))
  filename_ud50 <- file.path(base_dir, paste0(id, "_UD_50"))
  filename_points <- file.path(base_dir, paste0(id, "_locations"))
  
  writeVector(UD1w_pHREML_boot, filename_ud95, filetype="ESRI Shapefile", convex=FALSE, level.UD=0.95, level=0.95)
  writeRaster(UD1w_pHREML_boot,filename_ud95,format="GTiff",DF="CDF", level.UD=0.95, level=0.95)
  writeRaster(UD1w_pHREML_boot,filename_ud95,format="raster",DF="CDF", level.UD=0.95, level=0.95)
  writeVector(UD1w_pHREML_boot, filename_ud50, filetype="ESRI Shapefile", convex=FALSE, level.UD=0.5, level=0.95)
  writeRaster(UD1w_pHREML_boot,filename_ud50,format="GTiff",DF="CDF", level.UD=0.5, level=0.95)
  writeRaster(UD1w_pHREML_boot,filename_ud50,format="raster",DF="CDF", level.UD=0.5, level=0.95)
  
  # Write points to a shapefile
  writeVector(single_tag, filename_points, filetype="ESRI Shapefile")
  
  write.xlsx(home_range_data, file.path(base_dir, paste0(id, "_home_range_data.xlsx")))
}

write.xlsx(home_range_data, file.path(base_dir, "home_range_data_finished.xlsx"))