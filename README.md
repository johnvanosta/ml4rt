# ml4rt

## Machine Learning for Radio Telemetry

Tools to estimate wildlife locations from automated radio telemetry tower data

Includes:
- Text to csv converter for Lotek legacy format text files
- preprocessing data
- training ML models
- running models over raw data
- visualising results

This is to be built to allow for parameter optimisation. Parameters include:
- time period over which to average signals
- model selection and model parameters
- cross validation number
- spline method (or other location averaging functions)

To do:
- Need to add cross validation
- Capture and save models during the model training
- Consider engineering a feature that is something like the number of detections within n time (e.g. detection/5min)

Break up optimisation into:
- pre-processing (frequency, average signal strength, max signal strength, or include detection/time)
- model selection and parameter tuning
- Location averaging and smoothing functions (mean, lowess?, other?)

Data input requirements:
- GPS coordinates in WG84 datum
- Raw tower data, eventually in a csv format (some pre-processing scripts to support Lotek provided)
- Labelled data in the form of GPS locations of tags with start and end times

Process:
1. Pre-process data to required formats. Some assistance is provided by lotek_txt2csv.ipynb, then manual QA, if needed
2. simul_data_query.ipynb extracts the radio tower data that match the tag ID and time periods of the labelled GPS data. This script also splits the labelled data into training and testing sets (using a 80:20 split).
3. Train the model.