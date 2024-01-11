# Script to calculate BTF home ranges

# Clear environment
rm(list = ls())

################ Import data ########################
# Load libraries
pacman::p_load(dplyr, readr, openxlsx, readxl, ggplot2, lubridate, ctmm, data.table)

# Set working directory
setwd("~/GitHub/ml4rt/Example_data/Output/Predictions/MovementEcologyPaper")

base_dir <- "ctmm_output"

# Import the xlsx file for transect data
locs4ctmm <- read_excel("ctmm_input/ctmm_input_20240107.xlsx")

############## for testing!!!
# Extract the first two unique IDs and create a subset of locs4ctmm with only the first two unique IDs
#unique_ids <- unique(locs4ctmm$ID)[10:11]
#locs4ctmm <- filter(locs4ctmm, ID %in% unique_ids)

############### Calculate home ranges in ctmm ######################
# Following tutorial here (for error estimation): https://ctmm-initiative.github.io/ctmm/articles/error.html
# and for AKDE method: https://ecoisilva.github.io/AKDE_minireview/code/AKDE_R-tutorial.html

# Convert for input into ctmm
locs <- as.telemetry(locs4ctmm)

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
      FITS[[i]] <- ctmm.select(locs[[i]], GUESS[[i]], method = 'pHREML', verbose=TRUE)
      
      # Record success
      home_range_data <- rbind(home_range_data, data.frame(Name = current_name, Rows = nrow(locs[[i]]), FitSuccess = TRUE))
  }, error = function(e) {
      # Record failure
      home_range_data <- rbind(home_range_data, data.frame(Name = current_name, Rows = nrow(locs[[i]]), FitSuccess = FALSE))
  })
    
  # Save FITS after each iteration
  saveRDS(FITS, file = file.path(base_dir, "FITS_.rds"))
    
  # Save home_range_data as Excel after each iteration
  write.xlsx(home_range_data, file.path(base_dir, "home_range_data_20240107.xlsx"))
  }
}

# Define the indices where you want to select the second, third sublist, else return first
indices_2 <- c(11, 19, 20, 21, 23, 38, 52, 57, 64, 67, 69, 84, 97, 113, 120, 139, 141)
indices_3 <- c()
indices_4 <- c(15, 25, 26, 42, 58, 72, 89)
indices_5 <- c(33, 40, 49, 62, 93, 123)
indices_6 <- c(134)
indices_7 <- c()
indices_ctmm <- c(22, 28, 29, 31, 32, 34, 37, 39, 44, 46, 47, 53, 55, 59, 66, 71, 73, 76, 78, 79, 124)

# Create a new list with the selected sublists using lapply
FITS_select <- lapply(seq_along(FITS), function(i) {
  if (i %in% indices_2) {
    return(FITS[[i]][[2]])  # Select the sublists for special indices
  } else if (i %in% indices_3) {
    return(FITS[[i]][[3]])
  } else if (i %in% indices_4) {
    return(FITS[[i]][[4]])
  } else if (i %in% indices_5) {
    return(FITS[[i]][[5]])
  } else if (i %in% indices_6) {
    return(FITS[[i]][[6]])
  } else if (i %in% indices_7) {
    return(FITS[[i]][[7]])
  } else {
    return(FITS[[i]][[1]])  # Select the first sublist otherwise
  }
})

i <- 89
summary(FITS[[i]])
for (j in 1:9) {
  print(j)
  AKDES_single <- akde(locs[[i]], FITS[[i]][[j]], weights = TRUE)
  print(class(AKDES_single[[1]]))
}

# calculate AKDES on a consistent grid
AKDES <- list()
AKDES <- akde(locs,FITS_select,weights=TRUE,kernel="population")

saveRDS(AKDES, file = file.path(base_dir, "AKDES.rds"))

##### Import data if needed
FITS <- readRDS(file.path(base_dir, "FITS.rds"))
AKDES <- readRDS(file.path(base_dir, "AKDES.rds"))

# Filter out the ctmm indices to create a population KDE
AKDES_UD <- AKDES[-indices_ctmm]
locs_UD <- locs[-indices_ctmm]

# Calculate the population home range
MEAN_UD <- mean(AKDES_UD, weights=NULL, sample=TRUE)
plot(locs_UD, MEAN_UD)

saveRDS(MEAN_UD, file = file.path(base_dir, "PKDES.rds"))

# Extract home range areas for each tag
AREAS_UD_95 <- lapply(AKDES_UD,summary, level=0.95, level.UD=0.95, units=FALSE) # extract 95% areas
AREAS_UD_50 <- lapply(AKDES_UD,summary, level=0.95, level.UD=0.5, units=FALSE) # extract 50% areas

###### Compare home range areas to the sampling duration, number of points and number of days
# Manually check for a plateau

# Initialize an empty data frame
result_df <- data.frame(mean_UD_95=numeric(), mean_UD_50=numeric(), DOF_area=numeric(), DOF_bandwidth=numeric(), name=character(), 
                        sampling_period=numeric(), days_with_detections=integer(), 
                        number_detection=integer(), stringsAsFactors=FALSE)

# Loop through the AREAS_UD_95 list
for (i in seq_along(AREAS_UD_95)) {
  mean_UD_95_value_ha <- AREAS_UD_95[[i]]$CI[2]/10000 # Dividing by 10000 converts to hectares
  mean_UD_50_value_ha <- AREAS_UD_50[[i]]$CI[2]/10000 # Dividing by 10000 converts to hectares
  DOF_area_value <- AREAS_UD_95[[i]]$DOF[1]
  DOF_bandwidth_value <- AREAS_UD_95[[i]]$DOF[2]
  name_value <- names(locs_UD[i])
  
  # Calculate sampling period in days
  timestamps <- locs_UD[[i]]$timestamp
  sampling_period <- max(timestamps) - min(timestamps)
  
  # Calculate the unique number of days
  days_with_detections <- length(unique(as.Date(timestamps)))
  
  # Calculate the number of detections
  number_detection <- nrow(locs_UD[[i]])
  
  # Combine these values into a new row
  new_row <- data.frame(mean_UD_95_ha=mean_UD_95_value_ha, mean_UD_50_ha=mean_UD_50_value_ha, DOF_area=DOF_area_value, 
                        DOF_bandwidth=DOF_bandwidth_value, name=name_value, sampling_period=sampling_period, 
                        days_with_detections=days_with_detections, 
                        number_detection=number_detection, stringsAsFactors=FALSE)
  
  # Append this row to the data frame
  result_df <- rbind(result_df, new_row)
}

# Plot the results to look for plateau

# For effective sample size
median_UD_95_ha <- median(result_df$mean_UD_95_ha, na.rm = TRUE)
sample_size_plot <- ggplot(result_df, aes(x = DOF_area, y = mean_UD_95_ha)) +
  geom_point() +
  geom_hline(yintercept = median_UD_95_ha, linetype = "dashed", color = "blue") +  # Add dashed mean line
  scale_y_log10() +
  theme_bw() +
  labs(x = "Effective sample size (DOF)", y = "95% home range (hectares)")
plot(sample_size_plot)

ggsave("figures/sample_size_plot_20231225.png", sample_size_plot, units = 'cm', width = 16, height = 10, dpi = 300)

# Export home range data as an excel
write.xlsx(result_df, file.path(base_dir, "home_range_data_20231225.xlsx"))

# Use a ctmm meta function to get the average 95 and 50% home ranges
AKDES_UD_95_meta <- meta(AKDES_UD, verbose=TRUE, sort=FALSE, level=0.95, level.UD=0.95,
                      mean=TRUE, plot=FALSE)
print(AKDES_UD_meta)

AKDES_UD_50_meta <- meta(AKDES_UD, verbose=TRUE, sort=FALSE, level=0.95, level.UD=0.95,
                         mean=TRUE, plot=FALSE)
print(AKDES_UD_50_meta)

# Write to Excel
write.xlsx(AKDES_UD_95_meta, file = "AKDES_UD_95_meta_20240108.xlsx")
write.xlsx(AKDES_UD_50_meta, file = "AKDES_UD_50_meta_20240108.xlsx")

### Export individual an population home ranges as geotiff and esri shape
# Export individual home ranges
for (i in seq_along(AKDES_UD)) {
  name <- names(locs_UD[i])
  
  # ESRI Shapefile 95%
  filename_ud95_shp <- file.path(base_dir, paste0("GIS_files/", "UD_95_vector/", name))
  dir.create(filename_ud95_shp, recursive = TRUE)
  writeVector(AKDES_UD[[i]], filename=filename_ud95_shp, filetype="ESRI Shapefile", convex=FALSE, level.UD=0.95, level=0.95)
  
  # Geotiff 95%
  filename_ud95_geotiff <- file.path(base_dir, paste0("GIS_files/", "UD_95_raster/", name))
  dir.create(filename_ud95_geotiff, recursive = TRUE)
  writeRaster(AKDES_UD[[i]], filename=filename_ud95_geotiff, format="GTiff", level.UD=0.95, level=0.95)
  
  # ESRI Shapefile 50%
  filename_ud50_shp <- file.path(base_dir, paste0("GIS_files/", "UD_50_vector/", name))
  dir.create(filename_ud50_shp, recursive = TRUE)
  writeVector(AKDES_UD[[i]], filename=filename_ud50_shp, filetype="ESRI Shapefile", convex=FALSE, level.UD=0.50, level=0.95)
  
  # Geotiff 50%
  filename_ud50_geotiff <- file.path(base_dir, paste0("GIS_files/", "UD_50_raster/", name))
  dir.create(filename_ud50_geotiff, recursive = TRUE)
  writeRaster(AKDES_UD[[i]], filename=filename_ud50_geotiff, format="GTiff", level.UD=0.50, level=0.95)
}

#Export population home range
writeVector(MEAN_UD, filename="ctmm_output/GIS_files/population_UD_95_vector", filetype="ESRI Shapefile", convex=FALSE, level.UD=0.50, level=0.95)
writeRaster(MEAN_UD, filename="ctmm_output/GIS_files/population_UD_95_raster/", format="GTiff", level.UD=0.50, level=0.95)

