# Script to analyse BTF diel activity patterns

# Clear environment
rm(list = ls())

# Load libraries
pacman::p_load(dplyr, openxlsx, readxl, ggplot2, activity, lubridate, hms, DHARMa, sjPlot, glmmTMB, broom, gridExtra, MuMIn)

# Import the xlsx file for the individual locations, excluding ART data
indiv_locs_ex_art <- read_excel("BTF_Individual_Locations_20230718.xlsx")

# Filter dataframe to include only records where the activity status was known
activity_records <- indiv_locs_ex_art %>%
  filter(BTF_Activity_Report %in% c("Drinking", "Flying", "Perching/Preening", "Foraging", "Nesting")) %>%
  # Filter to only the radio tracking records, remove if all should be included
  filter(Method %in% c("Repeat observation", "Radio tracking")) %>%
  # Rename columns
  rename(
    ID = MetalBand,
    timestamp = CreationDateAEST,
    longitude = POINT_X,
    latitude = POINT_Y,
    activity = BTF_Activity_Report
  ) %>%
  # Combine 'Wet' and 'Early Wet' seasons
  mutate(Season = ifelse(Season %in% c("Wet", "Early Wet"), "Wet", Season)) %>%
  # Combine 'Flying' with 'Perching/Preening'
  mutate(activity = ifelse(activity %in% c("Flying", "Perching/Preening"), "Perching/Preening/Flying", activity))

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
activity_records$clock_solar_corrected <- tmp$solar*(24/(2*pi))
activity_records$month <- month(activity_records$timestamp, label=T)

plot(activity_records$solar, activity_records$clock)
plot(activity_records$clock_solar_corrected, activity_records$timestamp)

# Update the 'Season' column
#activity_records <- activity_records %>%
  #mutate(Season = ifelse(month %in% c("Jul", "Aug", "Sep", "Oct", "Nov"), "Dry", "Wet"))


# Adjust the variable for Daytime/Nighttime/Dawn/Dusk
activity_records <- activity_records %>%
  mutate(Time_of_day = case_when(
    floor(clock_solar_corrected) == 6 ~ "Twilight",
    clock_solar_corrected >= 7 & clock_solar_corrected < 18 ~ "Daytime",
    floor(clock_solar_corrected) == 18 ~ "Twilight",
    TRUE ~ "Nighttime"
  ))

# Calculate counts for each activity, grouped by time of day
activity_summary <- activity_records %>%
  group_by(activity, Time_of_day) %>%
  summarise(count = n(), .groups = 'drop')

# Calculate the total counts for each time of day period
totals <- activity_summary %>%
  group_by(Time_of_day) %>%
  summarise(total_count = sum(count), .groups = 'drop')

# Merge the totals back into the summary and calculate percentages
activity_summary <- activity_summary %>%
  left_join(totals, by = "Time_of_day") %>%
  mutate(percentage = (count / total_count) * 100)

# View the result
print(activity_summary)



# Calculate activity proportions in one hour windows, grouping by unique values in two columns: Season and activity, using the clock_solar_corrected column
activity_proportions <- activity_records %>%
  mutate(hour = floor(clock_solar_corrected)) %>%  # Get the hour from the clock_solar_corrected
  group_by(Season, hour, activity) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(Season, hour) %>%
  mutate(total_count = sum(count), proportion = count / total_count)

# Plot results with shaded areas for twilight and night
diel_plot <- ggplot(activity_proportions, aes(x = hour, y = proportion, fill = activity)) +
  # Twilight hours
  geom_rect(aes(xmin = 5.5, xmax = 6.5, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.2) +
  geom_rect(aes(xmin = 17.5, xmax = 18.5, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.2) +
  # Night hours
  geom_rect(aes(xmin = 4, xmax = 5.5, ymin = -Inf, ymax = Inf), fill = "black", alpha = 0.3) +
  geom_rect(aes(xmin = 18.5, xmax = 22, ymin = -Inf, ymax = Inf), fill = "black", alpha = 0.3) +
  # Day hours
  geom_rect(aes(xmin = 6.5, xmax = 17.5, ymin = -Inf, ymax = Inf), fill = "white", alpha = 0.3) +
  # Labels
  annotate("text", x = 6, y = max(activity_proportions$proportion) * 1.08, label = "Dawn", color = "black", fontface = "bold", size = 3) +
  annotate("text", x = 18, y = max(activity_proportions$proportion) * 1.08, label = "Dusk", color = "black", fontface = "bold", size = 3) +
  annotate("text", x = 12, y = max(activity_proportions$proportion) * 1.08, label = "Day", color = "black", fontface = "bold", size = 3) +
  annotate("text", x = 4.8, y = max(activity_proportions$proportion) * 1.08, label = "Night", color = "white", fontface = "bold", size = 3) +
  annotate("text", x = 20, y = max(activity_proportions$proportion) * 1.08, label = "Night", color = "white", fontface = "bold", size = 3) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = count), position = position_stack(vjust = 0.5), size = 3) +  # Add count labels
  facet_wrap(~Season, nrow = 3, labeller = labeller(Season = function(x) ifelse(x == "Dry", "Dry season", "Wet season"))) +
  labs(x = "Hour of day", y = "Proportion of sightings") +
  scale_fill_discrete(name = "Activity") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_x_continuous(limits = c(4, 22), breaks = seq(4, 22, by = 2), expand = c(0, 0))  # Set x-axis limits and tick marks every 2 hours

plot(diel_plot)

#ggsave("figures/diel_activity_20231229.png", plot = diel_plot, units = 'cm', width = 16, height = 16, dpi = 300)


# or a Cochran-Mantel-Haenszel test????
contingency_table <- xtabs(count ~ Season + hour + activity, data = activity_proportions)

# Display th table
ftable(contingency_table) 

### Woolf test for homogeneity of odds ratios across strata.
###   If significant, C-M-H test is not appropriate
#woolf_test(contingency_table)

# Run the Cochran-Mantel-Haenszel test
cmh_test_result <- mantelhaen.test(contingency_table)

# Output the result
print(cmh_test_result)

#### Looking at seasonal difference of activities only
activity_Time_of_day <- activity_records %>%
  mutate(
    Time_of_day = case_when(
      clock_solar_corrected >= 19 | clock_solar_corrected < 6 ~ "Night",
      clock_solar_corrected >= 6 & clock_solar_corrected < 7 ~ "Twilight",
      clock_solar_corrected >= 7 & clock_solar_corrected < 9 ~ "Early morning",
      clock_solar_corrected >= 9 & clock_solar_corrected < 11 ~ "Late morning",
      clock_solar_corrected >= 11 & clock_solar_corrected < 14 ~ "Midday",
      clock_solar_corrected >= 14 & clock_solar_corrected < 16 ~ "Early afternoon",
      clock_solar_corrected >= 16 & clock_solar_corrected < 18 ~ "Late afternoon",
      clock_solar_corrected >= 18 & clock_solar_corrected < 19 ~ "Twilight",
      TRUE ~ as.character(NA)
    )
  ) %>%
  filter(Time_of_day != "night", Time_of_day != "twilight") %>%
  group_by(Season, Time_of_day, activity) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(Season, Time_of_day) %>%
  mutate(total_count = sum(count), proportion = count / total_count)

# Create a complete set of combinations to fill in zeros for the model
all_combinations <- expand.grid(
  Season = unique(activity_records$Season),
  Time_of_day = c("Early morning", "Late morning", "Midday", "Early afternoon", "Late afternoon"),
  activity = unique(activity_records$activity)
)

expanded_activity <- left_join(all_combinations, activity_Time_of_day, by = c("Season", "Time_of_day", "activity"))

expanded_activity$count[is.na(expanded_activity$count)] <- 0
expanded_activity$total_count[is.na(expanded_activity$total_count)] <- 0
expanded_activity$proportion[is.na(expanded_activity$proportion)] <- 0

expanded_activity <- expanded_activity %>%
  mutate(Time_of_day = factor(Time_of_day, levels = c("Early morning", "Late morning", "Midday", "Early afternoon", "Late afternoon"))) %>%
  group_by(activity) %>%
  mutate(
    mean_proportion = mean(proportion, na.rm = TRUE),
    sd_proportion = sd(proportion, na.rm = TRUE),
    standardised_proportion = (proportion - mean_proportion) / sd_proportion
  ) %>%
  ungroup()

#m1 <- glmmTMB(proportion ~ activity/Season + activity:Season + activity:Time_of_day, 
#data=expanded_activity, 
#family=tweedie())

m1 <- lm(proportion ~ activity/Season + activity:Season + activity:Time_of_day, 
         data=expanded_activity)

summary(m1)

simulated_res <- simulateResiduals(fittedModel = m1)
plot(simulated_res)

plot_model(m1, line.color = "red", sort.est = TRUE, type = "std", show.values = TRUE, value.offset = .3)

# Extract model estimates
estimates <- tidy(m1, conf.int = TRUE)

#write.xlsx(estimates, file = "figures/glm_model_estimates_diel_patterns_20240103.xlsx")

nrow(expanded_activity)
r.squaredGLMM(m1)

# Calculate 95% Confidence Intervals
CI <- confint(m1)

# Print the Confidence Intervals
print(CI)

## glm cheat sheet: https://www.usabart.nl/eval/cs-generalized%20linear%20mixed%20effects.pdf


# Export the dataframe used for the stats
#write.xlsx(expanded_activity, file = "activity_summary_20240103.xlsx", rowNames=TRUE)

# Plots of different components of the model
theme_set(theme_bw())

plot_activity_time <- plot_model(m1,
                                 type = "eff",
                                 terms = c("Time_of_day", "activity"),
                                 show.legend = TRUE,
                                 axis.title = c("Time of day", "Proportion of observations"),
                                 title = ""         
) + 
  theme(legend.position = "top") +  # Move legend to top
  labs(color = "Activity")   

plot(plot_activity_time)

ggsave("figures/diel_activity_glm_20240119.png", plot = plot_activity_time, units = 'cm', width = 16, height = 12, dpi = 600)









### Backup code with less splits of time categories
activity_Time_of_day <- activity_records %>%
  mutate(
    Time_of_day = case_when(
      clock_solar_corrected >= 19 | clock_solar_corrected < 6 ~ "night",
      clock_solar_corrected >= 6 & clock_solar_corrected < 7 ~ "twilight",
      clock_solar_corrected >= 7 & clock_solar_corrected < 10 ~ "morning",
      clock_solar_corrected >= 10 & clock_solar_corrected < 14 ~ "midday",
      clock_solar_corrected >= 14 & clock_solar_corrected < 18 ~ "afternoon",
      clock_solar_corrected >= 18 & clock_solar_corrected < 19 ~ "twilight",
      TRUE ~ as.character(NA)
    )
  ) %>%
  filter(Time_of_day != "night", Time_of_day != "twilight") %>%
  group_by(Season, Time_of_day, activity) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(Season, Time_of_day) %>%
  mutate(total_count = sum(count), proportion = count / total_count)

# Create a complete set of combinations to fill in zeros for the model
all_combinations <- expand.grid(
  Season = unique(activity_records$Season),
  Time_of_day = c("morning", "midday", "afternoon"),
  activity = unique(activity_records$activity)
)

expanded_activity <- left_join(all_combinations, activity_Time_of_day, by = c("Season", "Time_of_day", "activity"))

expanded_activity$count[is.na(expanded_activity$count)] <- 0
expanded_activity$total_count[is.na(expanded_activity$total_count)] <- 0
expanded_activity$proportion[is.na(expanded_activity$proportion)] <- 0

expanded_activity <- expanded_activity %>%
  mutate(Time_of_day = factor(Time_of_day, levels = c("morning", "midday", "afternoon", "twilight", "night")))

#m1 <- glmmTMB(proportion ~ activity/Season + activity:Season + activity:Time_of_day, 
              #data=expanded_activity, 
              #family=tweedie())

m1 <- lm(proportion ~ activity/Season + activity:Season + activity:Time_of_day, 
         data=expanded_activity)

summary(m1)

simulated_res <- simulateResiduals(fittedModel = m1)
plot(simulated_res)


















#### Try splitting time of day in five categories






























# try to remove night and twilight data to assist with the glmm model

activity_Time_of_day <- activity_records %>%
  mutate(
    Time_of_day = case_when(
      clock_solar_corrected >= 19 | clock_solar_corrected < 6 ~ "night",
      clock_solar_corrected >= 6 & clock_solar_corrected < 7 ~ "twilight",
      clock_solar_corrected >= 7 & clock_solar_corrected < 10 ~ "morning",
      clock_solar_corrected >= 10 & clock_solar_corrected < 14 ~ "midday",
      clock_solar_corrected >= 14 & clock_solar_corrected < 18 ~ "afternoon",
      clock_solar_corrected >= 18 & clock_solar_corrected < 19 ~ "twilight",
      TRUE ~ as.character(NA)
    )
  ) %>%
  filter(Time_of_day != "night", Time_of_day != "twilight") %>%
  group_by(Season, Time_of_day, activity) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(Season, Time_of_day) %>%
  mutate(total_count = sum(count), proportion = count / total_count)

# Create a complete set of combinations to fill in zeros for the model
all_combinations <- expand.grid(
  Season = unique(activity_records$Season),
  Time_of_day = c("morning", "midday", "afternoon"),
  activity = unique(activity_records$activity)
)

expanded_activity <- left_join(all_combinations, activity_Time_of_day, by = c("Season", "Time_of_day", "activity"))

expanded_activity$count[is.na(expanded_activity$count)] <- 0
expanded_activity$total_count[is.na(expanded_activity$total_count)] <- 0
expanded_activity$proportion[is.na(expanded_activity$proportion)] <- 0

expanded_activity <- expanded_activity %>%
  mutate(Time_of_day = factor(Time_of_day, levels = c("morning", "midday", "afternoon", "twilight", "night")))

m1 <- glmmTMB(proportion ~ activity/Season + activity:Season + activity:Time_of_day, 
          data=expanded_activity, 
          family=gaussian())

summary(m1) ############ Make this the lastest model!!!

m2 <- glmmTMB(proportion ~ activity:Season + activity:Time_of_day, 
              data=expanded_activity, 
              family=gaussian())

summary(m2)

simulated_res <- simulateResiduals(fittedModel = m1)
plot(simulated_res)

anova(m1, m2)









######################## As seperate activities

foraging_proportions <- expanded_activity %>%
  filter(activity == "Foraging") %>%
  mutate(Time_of_day = factor(Time_of_day, levels = c("morning", "midday", "afternoon", "twilight", "night")))

m_forage <- glm(proportion ~ Season + Time_of_day, 
          data=foraging_proportions, 
          family=gaussian())

summary(m_forage)

simulated_res <- simulateResiduals(fittedModel = m_forage)
plot(simulated_res)

set_theme(base = theme_bw())

forage_estimates <- plot_model(m_forage, line.color = "red", sort.est = TRUE, type = "std", 
                               show.values = TRUE, value.offset = .3, show.intercept = TRUE,
                               title = "(A) Foraging")

plot(forage_estimates)

### Nesting estimates
nesting_proportions <- expanded_activity %>%
  filter(activity == "Nesting") %>%
  mutate(Time_of_day = factor(Time_of_day, levels = c("morning", "midday", "afternoon", "twilight", "night")))


m_nest <- glm(proportion ~ Season + Time_of_day, 
                data=nesting_proportions, 
                family=gaussian())

summary(m_nest)

nest_estimates <- plot_model(m_nest, line.color = "red", sort.est = TRUE, type = "std", 
                               show.values = TRUE, value.offset = .3, show.intercept = TRUE,
                               title = "(B) Nesting")

plot(nest_estimates)

# Filter for Drinking activity
drinking_proportions <- expanded_activity %>%
  filter(activity == "Drinking") %>%
  mutate(Time_of_day = factor(Time_of_day, levels = c("morning", "midday", "afternoon", "twilight", "night")))

# Fit the model
m_drink <- glm(proportion ~ Season + Time_of_day, 
               data=drinking_proportions, 
               family=gaussian())

# Summary
summary(m_drink)

# Plot the model
drink_estimates <- plot_model(m_drink, line.color = "red", sort.est = TRUE, type = "std", 
                              show.values = TRUE, value.offset = .3, show.intercept = TRUE,
                              title = "(C) Drinking")

plot(drink_estimates)


# Filter for Perching/Preening/Flying activity
perching_proportions <- expanded_activity %>%
  filter(activity == "Perching/Preening/Flying") %>%
  mutate(Time_of_day = factor(Time_of_day, levels = c("morning", "midday", "afternoon", "twilight", "night")))

# Fit the model
m_perch <- glm(proportion ~ Season + Time_of_day, 
               data=perching_proportions, 
               family=gaussian())

# Summary
summary(m_perch)

# Plot the model
perch_estimates <- plot_model(m_perch, line.color = "red", sort.est = TRUE, type = "std", 
                              show.values = TRUE, value.offset = .3, show.intercept = TRUE,
                              title = "(D) Perching/Preening/Flying")

plot(perch_estimates)

# Arrange the plots in a 2x2 grid
grid.arrange(forage_estimates, nest_estimates, 
             drink_estimates, perch_estimates, 
             ncol = 2, nrow = 2)







































# Adding predictions to the expanded dataset
expanded_activity$predicted <- predict(m1, newdata = expanded_activity, type = "response")

# Plotting with ggplot2
library(ggplot2)

ggplot(expanded_activity, aes(x = Time_of_day, y = predicted, group = Season, color = Season)) + 
  geom_line() + 
  facet_wrap(~ activity) +
  theme_minimal() +
  labs(y = "Predicted Proportion", x = "Time of Day")





# Predicted values
new_data <- expand.grid(Season = unique(expanded_activity$Season),
                        activity = unique(expanded_activity$activity),
                        Time_of_day = unique(expanded_activity$Time_of_day))
new_data$predicted <- predict(m1, newdata = new_data, type = "response")

# Using ggplot2 for plotting
library(ggplot2)
ggplot(new_data, aes(x = Time_of_day, y = predicted, color = Season)) + 
  geom_line() + 
  facet_wrap(~ activity) +
  theme_minimal()










activity_plot <- plot_model(m1,
           type = "pred",
           terms = c("Time_of_day", "activity"),
           show.p = TRUE
)

activity_plot <- activity_plot +
  facet_wrap(scales = "free_y")
  
plot(activity_plot)

foraging_proportions <- activity_proportions %>%
  filter(activity == "Foraging")

foraging_proportions$hour <- factor(foraging_proportions$hour)

m1 <- glm(proportion ~ Season + factor(hour), 
          data=foraging_proportions, family="gaussian")

m1 <- glmmTMB(proportion ~ Season + hour, 
              data=foraging_proportions, 
              family=gaussian())

summary(m1)

simulated_res <- simulateResiduals(fittedModel = m1)
plot(simulated_res)


# Plotting estimates from the model
plot_model(m1, line.color = "red", sort.est = FALSE, type = "std", show.values = TRUE, value.offset = .3)




levels(factor(foraging_proportions$hour))




specific_activity <- activity_proportions %>%
  filter(activity == "Foraging")

specific_activity <- activity_proportions %>%
  filter(activity == "Foraging") %>%
  group_by(Season) %>%
  summarise(
    count = sum(count),
    total_count = sum(total_count),
    .groups = 'drop'
  )

prop.test(sum(specific_activity$count), sum(specific_activity$total_count))








# Filter for a specific activity, e.g., 'Feeding'
specific_activity <- activity_records %>%
  filter(activity == "Foraging") %>%
  mutate(hour = floor(clock_solar_corrected)) %>%
  group_by(Season, hour) %>%
  summarise(count = n(), .groups = 'drop')

# Calculate total counts for each season
total_counts <- specific_activity %>%
  group_by(Season) %>%
  summarise(total = sum(count))

# Merge total counts with specific activity counts
merged_data <- merge(specific_activity, total_counts, by = "Season")

wet_data <- merged_data[merged_data$Season == "Wet",]
dry_data <- merged_data[merged_data$Season == "Dry",]

prop.test(c(sum(summer_data$count), sum(winter_data$count)),
          c(sum(summer_data$total), sum(winter_data$total)))














# Create a contingency table for Season and activity
contingency_table <- table(activity_proportions$activity, activity_proportions$Season)

print(contingency_table)

pairwise_chisq_gof_test(season_activity_table) 

chisq.multcomp(season_activity_table, p.method = "fdr")


season_activity_table <- xtabs(count ~ Season + activity, data = activity_proportions)

season_activity_table



# Create a contingency table with just Season and activity
season_activity_table <- xtabs(count ~ Season + activity, data = activity_proportions)

# Display the table
ftable(season_activity_table)

# Perform a Chi-square test to check for differences in activity counts across seasons
chi_square_test_result <- chisq.test(season_activity_table)

# Output the result of the Chi-square test
print(chi_square_test_result)

# Split data by season
season_split <- split(activity_proportions, activity_proportions$Season)

# Function to perform pairwise Fisher's Exact Test for each season
pairwise_fisher_test <- function(data) {
  pairwiseNominalIndependence(~ activity + Season,
                              data = data,
                              fisher = TRUE,
                              p.adjust.method = "bonferroni")
}

# Apply the function to each subset of data
pairwise_results <- lapply(season_split, pairwise_fisher_test)

# Output the pairwise comparison results
print(pairwise_results)





# Load necessary library
if (!require("dplyr")) install.packages("dplyr", dependencies=TRUE)
library(dplyr)



























# Fit an activity model 
m1 <- fitact(activity_records$solar, sample="model", reps=10) # change to 1000 after testing
plot(m1)
m1

# could fit a seperate model for each activity and compare the coefficient of overlap?
print(unique(activity_records$activity))
m1 <- fitact(activity_records$solar[activity_records$activity=="Drinking"], sample="model", reps=10)
m2 <- fitact(activity_records$solar[activity_records$activity=="Foraging"], sample="model", reps=10) # change to 1000 after testing
m3 <- fitact(activity_records$solar[activity_records$activity=="Nesting"], sample="model", reps=10)
m4 <- fitact(activity_records$solar[activity_records$activity=="Flying"], sample="model", reps=10)
m5 <- fitact(activity_records$solar[activity_records$activity=="Perching/Preening"], sample="model", reps=10)
plot(m1)
plot(m2)
plot(m3)
plot(m4)
plot(m5)

# Comparing coefficient of overlap against models. 0 = no overlap, 1 = high overlap. Get's a p value as well
compareCkern(m4, m5, reps = 10) # also change to 1000 after drinking

# Plot both on the same axis

plot(m1, yunit="density", data="none", las=1, lwd=2,
     tline=list(col="black", lwd=2), # Thick line 
     cline=list(lty=0)) # Supress confidence intervals

plot(m2, yunit="density", data="none", add=TRUE, 
     tline=list(col="red", lwd=2),
     cline=list(lty=0))

plot(m4, yunit="density", data="none", add=TRUE, 
     tline=list(col="green", lwd=2),
     cline=list(lty=0))

plot(m5, yunit="density", data="none", add=TRUE, 
     tline=list(col="blue", lwd=2),
     cline=list(lty=0))

legend("topright", c("Drinking", "Foraging", "Flying", "Perching/Preening"), col=1:4, lty=1, lwd=2)







### Also compare via season
# First pull out the month
activity_records$month <- month(activity_records$timestamp, label=T)

#Foraging activity
m_early_wet_forage <- fitact(activity_records$solar[activity_records$activity=="Foraging" &
                                               activity_records$month %in% c("Dec", "Jan", "Feb")], sample="model", reps=1000)

m_wet_forage <- fitact(activity_records$solar[activity_records$activity=="Foraging" &
                                                      activity_records$month %in% c("Mar", "Apr", "May")], sample="model", reps=1000)

m_dry_forage <- fitact(activity_records$solar[activity_records$activity=="Foraging" &
                                                activity_records$month %in% c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov")], sample="model", reps=1000)

# Drinking Activity
m_early_wet_drink <- fitact(activity_records$solar[activity_records$activity=="Drinking" &
                                                     activity_records$month %in% c("Dec", "Jan", "Feb")], sample="model", reps=1000)
m_wet_drink <- fitact(activity_records$solar[activity_records$activity=="Drinking" &
                                               activity_records$month %in% c("Mar", "Apr", "May")], sample="model", reps=1000)
m_dry_drink <- fitact(activity_records$solar[activity_records$activity=="Drinking" &
                                               activity_records$month %in% c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov")], sample="model", reps=1000)

# Flying Activity
m_early_wet_fly <- fitact(activity_records$solar[activity_records$activity=="Flying" &
                                                   activity_records$month %in% c("Dec", "Jan", "Feb")], sample="model", reps=1000)
m_wet_fly <- fitact(activity_records$solar[activity_records$activity=="Flying" &
                                             activity_records$month %in% c("Mar", "Apr", "May")], sample="model", reps=1000)
m_dry_fly <- fitact(activity_records$solar[activity_records$activity=="Flying" &
                                             activity_records$month %in% c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov")], sample="model", reps=1000)

# Perching/Preening Activity
m_early_wet_perch <- fitact(activity_records$solar[activity_records$activity=="Perching/Preening" &
                                                     activity_records$month %in% c("Dec", "Jan", "Feb")], sample="model", reps=1000)
m_wet_perch <- fitact(activity_records$solar[activity_records$activity=="Perching/Preening" &
                                               activity_records$month %in% c("Mar", "Apr", "May")], sample="model", reps=1000)
m_dry_perch <- fitact(activity_records$solar[activity_records$activity=="Perching/Preening" &
                                               activity_records$month %in% c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov")], sample="model", reps=1000)

# Initialize a dataframe to store results
overlap_results <- data.frame(Activity1 = character(), Activity2 = character(), Season = character(), Overlap = numeric())

# Function to add results to the dataframe
add_overlap_result <- function(activity1, activity2, season, overlap) {
  new_row <- data.frame(Activity1 = activity1, Activity2 = activity2, Season = season, Overlap = overlap)
  overlap_results <<- rbind(overlap_results, new_row)
}

# Calculate overlaps and add to dataframe

# Early Wet Season
add_overlap_result("Forage", "Drink", "Early Wet", compareCkern(m_early_wet_forage, m_early_wet_drink, reps = 10))
add_overlap_result("Forage", "Fly", "Early Wet", compareCkern(m_early_wet_forage, m_early_wet_fly, reps = 10))
add_overlap_result("Forage", "Perch", "Early Wet", compareCkern(m_early_wet_forage, m_early_wet_perch, reps = 10))
add_overlap_result("Drink", "Fly", "Early Wet", compareCkern(m_early_wet_drink, m_early_wet_fly, reps = 10))
add_overlap_result("Drink", "Perch", "Early Wet", compareCkern(m_early_wet_drink, m_early_wet_perch, reps = 10))
add_overlap_result("Fly", "Perch", "Early Wet", compareCkern(m_early_wet_fly, m_early_wet_perch, reps = 10))

# Wet Season
add_overlap_result("Forage", "Drink", "Wet", compareCkern(m_wet_forage, m_wet_drink, reps = 10))
add_overlap_result("Forage", "Fly", "Wet", compareCkern(m_wet_forage, m_wet_fly, reps = 10))
add_overlap_result("Forage", "Perch", "Wet", compareCkern(m_wet_forage, m_wet_perch, reps = 10))
add_overlap_result("Drink", "Fly", "Wet", compareCkern(m_wet_drink, m_wet_fly, reps = 10))
add_overlap_result("Drink", "Perch", "Wet", compareCkern(m_wet_drink, m_wet_perch, reps = 10))
add_overlap_result("Fly", "Perch", "Wet", compareCkern(m_wet_fly, m_wet_perch, reps = 10))

# Dry Season
add_overlap_result("Forage", "Drink", "Dry", compareCkern(m_dry_forage, m_dry_drink, reps = 10))
add_overlap_result("Forage", "Fly", "Dry", compareCkern(m_dry_forage, m_dry_fly, reps = 10))
add_overlap_result("Forage", "Perch", "Dry", compareCkern(m_dry_forage, m_dry_perch, reps = 10))
add_overlap_result("Drink", "Fly", "Dry", compareCkern(m_dry_drink, m_dry_fly, reps = 10))
add_overlap_result("Drink", "Perch", "Dry", compareCkern(m_dry_drink, m_dry_perch, reps = 10))
add_overlap_result("Fly", "Perch", "Dry", compareCkern(m_dry_fly, m_dry_perch, reps = 10))

# View the combined results
print(overlap_results)

# Export the dataframe to an Excel file
write.xlsx(overlap_results, file = "diel_activity_overlap_20231226.xlsx", rowNames=TRUE)




overlap_early_wet_forage_drink <- compareCkern(m_early_wet_forage, m_early_wet_drink, reps = 10)

overlap_early_wet_forage_drink[1]




plot(m_early_wet_forage, yunit="density", data="none", las=1, lwd=2,
     tline=list(col="black", lwd=2), # Thick line 
     cline=list(lty=0)) # Supress confidence intervals

plot(m_wet_forage, yunit="density", data="none", add=TRUE, 
     tline=list(col="red", lwd=2),
     cline=list(lty=0))

plot(m_dry_forage, yunit="density", data="none", add=TRUE, 
     tline=list(col="green", lwd=2),
     cline=list(lty=0))

legend("topright", c("Early wet", "Wet", "Dry"), col=1:3, lty=1, lwd=2)
