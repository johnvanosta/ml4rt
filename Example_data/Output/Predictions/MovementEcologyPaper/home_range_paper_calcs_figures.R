rm(list = ls())

pacman::p_load(dplyr, readxl, ggplot2, DHARMa, sjPlot, glmmTMB, gridExtra, lubridate, GGally, MuMIn)

setwd("~/GitHub/ml4rt/Example_data/Output/Predictions/MovementEcologyPaper")

# Load the data
UD_data <- read_excel("Supplementary_2_home_range_data.xlsx", sheet = 'home_range_data')

colnames(UD_data)

# Import the excel file for site weather data
site_weather_data <- read_excel("weather_data/Weather_data_compiled_20230919.xlsx", sheet = "Daily")

# Import GTRE data
gtre_data <- read_excel("GIS_Output/UD95_gtre_per_tag.xlsx")


#### Calculate summary statistics
# Define the list of columns to analyze
columns_to_analyze <- c("sampling_period", "number_detection", "mean_UD_95_ha", "mean_UD_50_ha") #add 'MCP' when available

# Function to calculate and print statistics
calculate_and_print_stats <- function(data, col) {
  mean_val <- mean(data[[col]], na.rm = TRUE)
  min_val <- min(data[[col]], na.rm = TRUE)
  max_val <- max(data[[col]], na.rm = TRUE)
  median_val <- median(data[[col]], na.rm = TRUE)
  sd_val <- sd(data[[col]], na.rm = TRUE)
  
  cat("Statistics for", col, ":\n")
  cat("Mean: ", mean_val, "\n")
  cat("Min: ", min_val, "\n")
  cat("Max: ", max_val, "\n")
  cat("Median: ", median_val, "\n")
  cat("Standard Deviation: ", sd_val, "\n\n")
}

# Loop through each column and calculate statistics
for (col in columns_to_analyze) {
  calculate_and_print_stats(UD_data, col)
}

# Plot home range against DOF_area and fit a linear model to show change
# Fit linear models
model_95 <- lm(log(mean_UD_95_ha) ~ log(DOF_area), data = UD_data)
model_50 <- lm(log(mean_UD_50_ha) ~ log(DOF_area), data = UD_data)

summary(model_95)
summary(model_50)

# Create ggplot objects
plot_95 <- ggplot(UD_data, aes(x = DOF_area, y = mean_UD_95_ha)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, color = "darkgrey") +
  labs(x = "Effective sample size",
       y = "95% kernel density home range (ha)") +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()

plot_50 <- ggplot(UD_data, aes(x = DOF_area, y = mean_UD_50_ha)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, color = "darkgrey") +
  labs(x = "Effective sample size",
       y = "50% kernel density home range (ha)") +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()

# Combine the plots side by side
combined_plot <- grid.arrange(plot_95, plot_50, ncol = 2)

# Export the plot
#ggsave("figures/UD_vs_DOF.png", plot = combined_plot, width = 16, height = 8, units = "cm", dpi = 300)



######## Data formatting for a (g)lm

# Join weather data on date
UD_data_lm <- UD_data %>%
  mutate(Season = ifelse(Season == "Early Wet", "Wet", Season),
         end_date = Date_of_transmitter_attachment + days(ceiling(sampling_period)))

# For each row in UD_data_lm: calculate the sum of rain_mm, average of max_temp_deg_c, min_temp_deg_c between the Date_of_transmitter_attachment and end_date
# Function to calculate sum of rain and average temperatures in a given period
calc_weather_stats <- function(start_date, end_date, data){
  relevant_data <- data[data$date >= start_date & data$date <= end_date, ]
  sum_rain <- sum(relevant_data$rain_mm, na.rm = TRUE)
  avg_max_temp <- mean(relevant_data$max_temp_deg_c, na.rm = TRUE)
  avg_min_temp <- mean(relevant_data$min_temp_deg_c, na.rm = TRUE)
  
  return(c(sum_rain, avg_max_temp, avg_min_temp))
}

# Apply the function to each row in UD_data_lm
UD_data_lm <- UD_data_lm %>%
  rowwise() %>%
  mutate(
    # Directly calculate and assign the values
    Total_rain_tag_period = calc_weather_stats(Date_of_transmitter_attachment, end_date, site_weather_data)[1],
    Avg_max_temp_tag_period = calc_weather_stats(Date_of_transmitter_attachment, end_date, site_weather_data)[2],
    Avg_min_temp_tag_period = calc_weather_stats(Date_of_transmitter_attachment, end_date, site_weather_data)[3]
  ) %>%
  ungroup()


# For each row in UD_data_lm: calculate the sum of rain_mm for the following periods prior to the Date_of_transmitter_attachment: 6 weeks prior, 6 to 12 weeks prior, 12 to 18 weeks prior, and 6 to 18 weeks prior.

# Function to compute start date for a given period
get_start_date <- function(end_date, weeks_prior) {
  return(as.Date(end_date) - weeks_prior * 7)
}

# Apply the function to each row in UD_data_lm
UD_data_lm <- UD_data_lm %>%
  rowwise() %>%
  mutate(
    Total_rain_0to6_weeks = calc_weather_stats(get_start_date(Date_of_transmitter_attachment, 6), Date_of_transmitter_attachment, site_weather_data)[1],
    Total_rain_6to12_weeks = calc_weather_stats(get_start_date(Date_of_transmitter_attachment, 12), get_start_date(Date_of_transmitter_attachment, 6), site_weather_data)[1],
    Total_rain_12to18_weeks = calc_weather_stats(get_start_date(Date_of_transmitter_attachment, 18), get_start_date(Date_of_transmitter_attachment, 12), site_weather_data)[1],
    Total_rain_6to18_weeks = calc_weather_stats(get_start_date(Date_of_transmitter_attachment, 18), Date_of_transmitter_attachment, site_weather_data)[1]
  ) %>%
  ungroup()

# calculate area of preferred REs within the 95% home range of each tag
# Adjust RE_Code
gtre_data <- gtre_data %>%
  mutate(
    RE = case_when(
      RE_Code != "Non-remnant" ~ sapply(strsplit(gsub("[a-zA-Z]", "", RE_Code), "/"), `[`, 1),
      TRUE ~ RE_Code
    ),
    RE = case_when(
      RE %in% c("10.3.6", "10.3.61", "11.3.10") ~ "E. brownii W.",
      RE == "10.5.5" ~ "E. melanophloia W.",
      RE %in% c("10.3.15", "11.3.3", "11.3.25", "11.3.27", "10.3.13", "10.3.14") ~ "E. camaldulensis W.",
      RE %in% c("10.10.1", "10.7.3") ~ "A. shirleyi O.F.",
      RE %in% c("10.3.4", "10.4.5","11.3.5", "11.4.6") ~ "A. cambagei W.",
      RE %in% c("11.4.9", "10.4.3") ~ "A. harpophylla W.",
      RE %in% c("10.7.4", "10.7.5") ~ "E. persistens W.",
      RE %in% c("10.3.12", "10.5.2") ~ "Corymbia spp. W.",
      RE == "10.5.1" ~ "E. similis W.",
      RE == "10.7.7" ~ "M. tamariscina woodland",
      TRUE ~ RE
    )
  )

unique(gtre_data$RE)

# Calculate the area of "E. melanophloia W." and combined ["E. similis W.", "E. melanophloia W.", "E. brownii W."] for each ID in UD_data_lm
# Calculate areas for "E. melanophloia W."
emelanophloia_areas <- gtre_data %>%
  filter(RE == "E. melanophloia W.") %>%
  group_by(ID) %>%
  summarise(Area_Emelanophloia = sum(AreaHA, na.rm = TRUE))

# Calculate combined areas for ["E. similis W.", "E. melanophloia W.", "E. brownii W."]
pref_RE_areas <- gtre_data %>%
  filter(RE %in% c("E. similis W.", "E. melanophloia W.", "E. brownii W.")) %>%
  group_by(ID) %>%
  summarise(Area_pref_RE = sum(AreaHA, na.rm = TRUE))

# Merge these calculations with UD_data_lm
UD_data_lm <- UD_data_lm %>%
  left_join(emelanophloia_areas, by = "ID") %>%
  left_join(pref_RE_areas, by = "ID")%>%
  # Change any na values to zero
  mutate(
    Area_Emelanophloia = ifelse(is.na(Area_Emelanophloia), 0, Area_Emelanophloia),
    Area_pref_RE = ifelse(is.na(Area_pref_RE), 0, Area_pref_RE),
    prop_Emelanophloia = Area_Emelanophloia/mean_UD_95_ha,
    prop_pref_RE = Area_pref_RE/mean_UD_95_ha)

### Data exploration

# Plot 95% UD by season
ggplot(UD_data_lm, aes(x = Season, y = mean_UD_95_ha)) + 
  geom_violin(trim = FALSE) +
  labs(title = "Violin Plot of UD_95_ha by Season",
       x = "Season",
       y = "UD_95_ha") +
  theme_bw()

# Plot distribution of UD_95_ha
ggplot(UD_data_lm, aes(x = mean_UD_95_ha)) +
  geom_histogram(binwidth = 20, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of UD_95_ha", x = "mean_UD_95_ha", y = "Frequency")

# Selecting the relevant variables from the dataset including 'Season'
selected_data <- UD_data_lm[, c("Total_rain_tag_period", 
                                "Avg_max_temp_tag_period", 
                                "Total_rain_0to6_weeks", 
                                "Total_rain_6to18_weeks", 
                                "Season",
                                "prop_Emelanophloia",
                                "prop_pref_RE",
                                "mean_UD_95_ha")]

# Creating the ggpairs plot
ggpairs(selected_data)

# Plot 95% UD by prop_Emelanophloia
ggplot(UD_data_lm, aes(x = prop_Emelanophloia, y = mean_UD_95_ha)) + 
  geom_jitter() + 
  labs(title = "scatterplot of UD_95_ha by prop e melanophloia",
       x = "e melanophloia",
       y = "UD_95_ha") +
  theme_bw()

# Notes
# Min and max tempt highly correlated (0.918), exclude min temp.
# Rain 12 to 18 and 6 to 18 weeks highly correlated (0.751), exclude 6to18 weeks

# Additional data cleaning step/s

# Exclude outliers of UD_95_ha
#UD_data_lm <- subset(UD_data_lm, UD_95_ha >= quantile(UD_95_ha, 0.25) - 1.5 * IQR(UD_95_ha) & UD_95_ha <= quantile(UD_95_ha, 0.75) + 1.5 * IQR(UD_95_ha))

# Assessing difference in home ranges among seasons
# Exploring models

m_max <- glmmTMB(mean_UD_95_ha ~ Total_rain_tag_period +
                 Avg_max_temp_tag_period +
                 Total_rain_0to6_weeks +
                 Total_rain_6to12_weeks +
                 Total_rain_12to18_weeks +
                 prop_Emelanophloia +
                 prop_pref_RE +
                 Total_rain_0to6_weeks:Total_rain_6to12_weeks +
                 Total_rain_0to6_weeks:Total_rain_12to18_weeks +
                 Total_rain_6to12_weeks:Total_rain_12to18_weeks +
                 (1|Site),
                 data = UD_data_lm, family = Gamma(link = "log"))
summary(m_max)

m_all_no_interac <- glmmTMB(mean_UD_95_ha ~ Total_rain_tag_period +
                 Avg_max_temp_tag_period +
                 Total_rain_0to6_weeks +
                 Total_rain_6to18_weeks +
                 (1|Site),
                 data = UD_data_lm, family = Gamma(link = "log"))
summary(m_all_no_interac)

m_season_cat <- glmmTMB(UD_95_ha ~ Season, data = UD_data_lm, family = Gamma(link = "log"))

summary(m_season_cat)

m_temp <- glmmTMB(UD_95_ha ~ Avg_max_temp_tag_period,
                 data = UD_data_lm, family = Gamma(link = "log"))
summary(m_temp)

m_rain <- glmmTMB(UD_95_ha ~ Total_rain_tag_period +
                    Total_rain_0to6_weeks +
                    Total_rain_6to12_weeks +
                    Total_rain_12to18_weeks,
                  data = UD_data_lm, family = Gamma(link = "log"))
summary(m_rain)

model.sel(m_max, m_all_no_interac, m_season_cat, m_temp, m_rain)

# Run stats and plots on the best model
best_model <- m_all_no_interac

simulated_res <- simulateResiduals(fittedModel = best_model)
plot(simulated_res)

# Extract model estimates and write to a text file
model_summary <- capture.output(summary(best_model))
writeLines(model_summary, "figures/home_range_model_202401114.txt")


nrow(UD_data_lm)
r.squaredGLMM(m_all_no_interac)
