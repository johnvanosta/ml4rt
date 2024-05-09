import pandas as pd

# Set the input and output file paths
input_file = 'C:/Users/John/Google Drive/E2M/BTFRP/Data/Radio Telemetry/MATLAB/RT_MASTER_DATA/RTdatMaster20230317.csv'
output_file = 'Example_data/Input/Lotek_combined_csv/sampled_data.csv'

# Set the number of lines to sample
sample_size = 50000

# Read the input file
df = pd.read_csv(input_file)

# Sample the data
df_sample = df.sample(n=sample_size)

# Export the sampled data to a CSV file
df_sample.to_csv(output_file, index=False)

print("Sampling and CSV export complete!")
