rm(list = ls())

pacman::p_load(dplyr, readxl, openxlsx)

setwd("~/GitHub/ml4rt/Example_data/Output/Predictions/MovementEcologyPaper")

csv_path <- "GIS_Output/CSV"

# Reading all CSV files
file_list <- list.files(path = csv_path, pattern = "*.csv", full.names = TRUE)

# Function to read and transform each file
process_file <- function(file_path) {
  csv_name <- basename(file_path)
  # Replace the first "_" in csv_name with "-"
  csv_name <- sub("_", "-", csv_name)
  # Read CSV file
  df <- read.csv(file_path)
  # Split csv_name by "_" and keep the first column labeled as "ID"
  df$ID <- strsplit(csv_name, "_")[[1]][1]
  return(df)
}

# Applying the function to each file and combining the results
combined_data <- do.call(rbind, lapply(file_list, process_file))

# Save to GIS_Output as an excel file
write.xlsx(combined_data, "GIS_Output/UD95_gtre_per_tag.xlsx")
