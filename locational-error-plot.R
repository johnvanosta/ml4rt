library(readxl)
library(ggplot2)

excel_path <- "C:/Users/s5236256/Documents/GitHub/ml4rt/Example_data/Output/Predictions/UTM_predictions_combined_dtypes_input_20230701.xlsx"

predict_df <- read_excel(excel_path)

head(predict_df)

predict_df$log_error_m <- log(predict_df$error_m, base = 10)

# Filter the data by 'training_data_type'
dtype_combined_df <- subset(predict_df, training_data_type == 'Combined')

# Create the plot with points and lines
ggplot(data = dtype_combined_df, aes(x = easting_error, y = northing_error, color = Data_type)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +
  labs(x = "Northing error (m)", y = "Easting error (m)") +
  scale_color_discrete(name = "Testing dataset") +
  theme_bw() +
  theme(legend.position = "top")

# Create the density plot with separate geom_rug for each Data_type and custom color palette
ggplot(data = dtype_combined_df, aes(x = error_m, fill = Data_type)) +
  geom_density(alpha = 0.5, color = "black", bw = 150) +
  geom_rug(data = subset(predict_df, Data_type == "Tracked bird"), sides = "b", color = "#FC4E07") +
  geom_rug(data = subset(predict_df, Data_type == "Simulated bird"), sides = "t", color = "#00AFBB") +
  labs(x = "Locational error (m)", y = "Density") +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07"), name = "Testing dataset") +
  theme_bw() +
  theme(legend.position = "top")
