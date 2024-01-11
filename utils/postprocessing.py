#Convert locations predictions back to easting northings

import numpy as np

def postprocess_data(prediction_data, tower_locs):
    # Create a dictionary of the coordinates of the towers
    offset_dict = tower_locs.set_index('TowerID').to_dict()
    point_x = offset_dict['POINT_X']
    point_y = offset_dict['POINT_Y']
    zone_number = offset_dict['zone_number']
    zone_letter = offset_dict['zone_letter']

    # Change predicted x/y offset values to their respective easting/northing considering the location of the tower
    prediction_data['easting_pred'] = prediction_data['xOffset_pred'] + prediction_data['TowerID'].map(point_x).fillna(0)
    prediction_data['northing_pred'] = prediction_data['yOffset_pred'] + prediction_data['TowerID'].map(point_y).fillna(0)
    
    # Add UTM zone details
    prediction_data['zone_number'] = prediction_data['TowerID'].map(zone_number).fillna(0)
    prediction_data['zone_letter'] = prediction_data['TowerID'].map(zone_letter).fillna(0)
    
    return prediction_data

def location_averaging(prediction_data):
    # Get the columns that start with 'ant' and end with '_count'
    ant_columns = [col for col in prediction_data.columns if col.startswith('ant') and col.endswith('_count')]

    # Sum the values of the 'ant' columns into a single column 'ant_sum'
    prediction_data['Signal_count'] = prediction_data[ant_columns].sum(axis=1)

    # Group by columns and calculate the desired aggregations
    prediction_data = (prediction_data.groupby(['DateTime', 'TagID'], as_index=False)
        .agg({'easting':'first',
            'northing':'first',
            'easting_pred':'mean',
            'northing_pred':'mean',
            'TowerID': lambda x: x.nunique(),
            'Data_type': 'first',
            'Signal_count':'sum',  # Total sum of 'ant' columns
            'xOffset':'mean',
            'yOffset':'mean'
            })
    )

    prediction_data.rename(columns={'TowerID': 'Tower_count'}, inplace=True) # Rename TowerID column to a more appropriate name

    return prediction_data

def calculate_error(test_location_estimates):
    # Calculate the error of location predictions
    test_location_estimates['easting_error'] = test_location_estimates['easting_pred'] - test_location_estimates['easting']
    test_location_estimates['northing_error'] = test_location_estimates['northing_pred'] - test_location_estimates['northing']

    # Calculate the Eucledian distance between the predicted and actual locations
    test_location_estimates['error_m'] = np.sqrt((test_location_estimates['easting_error']) ** 2
                        + (test_location_estimates['northing_error']) ** 2)

    # Calculate the average Eucledian distance from the towers
    test_location_estimates['mean_distance_from_tower'] = np.sqrt((test_location_estimates['xOffset']) ** 2
                        + (test_location_estimates['yOffset']) ** 2)
    
    return test_location_estimates