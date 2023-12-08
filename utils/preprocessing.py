import numpy as np
import pandas as pd

#### Note to self: Remove 'Point_ID', 'Tag_type', 'Interval_seconds' prior to publishing

def training_preprocess_sub(data_preprocessed):
    # group by datetime, tag, tower and antenna, compute mean power and std power, pivot to antennas
    data_preprocessed = (
        data_preprocessed.groupby(['DateTime', 'TowerID', 'TagID', 'Antenna', 'Data_type', 'POINT_X', 'POINT_Y', 'Point_ID', 'Tag_type', 'Interval_seconds'])['Power']
        .agg(['mean', 'count', np.std])
        .reset_index()
    )

    # Pivot table with antennas as columns
    data_preprocessed = (
        data_preprocessed.pivot_table(
            index=['DateTime', 'TowerID', 'TagID', 'Data_type', 'POINT_X', 'POINT_Y', 'Point_ID', 'Tag_type', 'Interval_seconds'],
            columns='Antenna',
            values=['mean', 'count', 'std']
        )
        .reset_index()
    )
    return data_preprocessed

def prediction_preprocess_sub(data_preprocessed):
    # group by datetime, tag, tower and antenna, compute mean power and std power, pivot to antennas
    data_preprocessed = (
        data_preprocessed.groupby(['DateTime', 'TowerID', 'TagID', 'Antenna'])['Power']
        .agg(['mean', 'count', np.std])
        .reset_index()
    )

    # Pivot table with antennas as columns
    data_preprocessed = (
        data_preprocessed.pivot_table(
            index=['DateTime', 'TowerID', 'TagID'],
            columns='Antenna',
            values=['mean', 'count', 'std']
        )
        .reset_index()
    )
    return data_preprocessed

## Backup of prediction_preprocess before change on 4/12/2023
# def prediction_preprocess_sub(data_preprocessed):
#     # group by datetime, tag, tower and antenna, compute mean power and std power, pivot to antennas
#     data_preprocessed = (
#         data_preprocessed.groupby(['DateTime', 'TowerID', 'TagID', 'Antenna', 'Point_ID', 'Tag_type', 'Interval_seconds'])['Power']
#         .agg(['mean', 'count', np.std])
#         .reset_index()
#     )

#     # Pivot table with antennas as columns
#     data_preprocessed = (
#         data_preprocessed.pivot_table(
#             index=['DateTime', 'TowerID', 'TagID', 'Point_ID', 'Tag_type', 'Interval_seconds'],
#             columns='Antenna',
#             values=['mean', 'count', 'std']
#         )
#         .reset_index()
#     )
#     return data_preprocessed

def preprocess(input_data, freq, routine):
    # make column with the datetime to nearest 'freq' value (e.g. 5min)
    print("up to preprocess")
    data_preprocessed = input_data.assign(DateTime = input_data['DateAndTime'].dt.floor(freq=freq))
    print("created DateTime")
    # Create a unique list of antennas
    antennas = data_preprocessed['Antenna'].unique()

    if routine == 'training':
        data_preprocessed = training_preprocess_sub(data_preprocessed)
    elif routine == 'prediction':
        print("down routine prediction route")
        data_preprocessed = prediction_preprocess_sub(data_preprocessed)
        print("prediction route subprocess complete")
    else:
        raise ValueError("Invalid value for 'routine'. Please specify either 'training' or 'prediction'.")

    # Rename columns
    data_preprocessed.columns = [f"{col[0]}{col[1]}" if col[1] != "" else col[0] for col in data_preprocessed.columns.values]

    # Adjust column names for antennas
    predictors = []
    col_mapping = {}
    for antenna in antennas:
        col_mapping[f'mean{antenna}'] = f'ant{antenna}_mean'
        col_mapping[f'count{antenna}'] = f'ant{antenna}_count'
        col_mapping[f'std{antenna}'] = f'ant{antenna}_std'

        # Add the created columns to the predictors list
        predictors.extend([f'ant{antenna}_mean', f'ant{antenna}_count', f'ant{antenna}_std'])

    data_preprocessed = data_preprocessed.rename(columns=col_mapping)
    
    # Calculate the mean std and total count across the antennas
    mean_std_columns = [f'ant{antenna}_std' for antenna in antennas]
    total_count_columns = [f'ant{antenna}_count' for antenna in antennas]
    data_preprocessed['mean_std'] = data_preprocessed[mean_std_columns].mean(axis=1)
    data_preprocessed['total_count'] = data_preprocessed[total_count_columns].sum(axis=1)

    # Fill missing values with 0
    data_preprocessed = data_preprocessed.fillna(value=0)

    return data_preprocessed, predictors