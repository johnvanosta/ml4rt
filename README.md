# ml4rt

## Machine Learning for Radio Telemetry

Tools to estimate wildlife locations from automated radio telemetry tower data

Includes:
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