# ml4rt: Machine Learning for Radio Telemetry

This repository contains the code base and supporting data to reproduce the findings of the paper: 'Comparison of localisation methods for the automated radio telemetry of wildlife' (herein referred to as 'the Paper')

The repository also contains instructions and tools to apply the method to other studies.

## Code base
The code base includes:
- train_model.ipynb: Uses the H20 AutoML package to fit models to the training data and evaluate accuracy using a test dataset.
- predict.ipynb: Applies the trained model to unlabelled data (inference)
- mechanistic_models.ipynb: Runs two alternative localisation approaches, which are linear regression and biangulation, as described in the Paper.
- requirements.txt: Python package requirements to run the code base

## Supplementary files and reproduction of the paper's results
The folder 'SBTF_data' contains the data, figures and R code to reproduce the results of the Paper. Specifically, these include:
- Input data, including, training and testing data and information on the radio tower locations.
- Output from the location fingerprinting, linear regression and biangulation methods compared in the Paper.
- Data to support the analysis of the impact of sample size on positional error.
- Figures from the paper
- R code to repeat all statistical models and figure production.

## Using the model on your own data
### Getting started
1. Install Java, which is required by H20 (used as the engine to train the machine learning models):
    - Version 17 recommended: https://www.oracle.com/java/technologies/javase/jdk17-archive-downloads.html
    - Following recommendations from H20: https://docs.h2o.ai/h2o/latest-stable/h2o-docs/welcome.html#java-requirements
2. Clone this repository (https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository)
3. Open repository in your code editor. We have tested this repository using Microsoft's Visual Studio Code (https://code.visualstudio.com/). If using Visual Studio Code, ensure the 'Python' and 'Jupyter' extensions are installed (https://code.visualstudio.com/docs/datascience/jupyter-notebooks).
4. Setup the python environment by installing the package requirements identified within the requirements.txt file contained within the repository. The steps to setup a python environment in Visual Studio Code are described here: https://code.visualstudio.com/docs/python/environments, or more generally for python described here: https://packaging.python.org/en/latest/guides/installing-using-pip-and-virtual-environments/. This will download the necessary python packages to support the below code.

### Process to localise transmitter positions
#### Train a model
1. Ensure receiver data are formatted consistent with the example 'test_data.xlsx' and 'train_data.xlsx' saved at path: SBTF_data\input_data\train_test_data
2. Ensure data on the radio tower locations and groups are formatted consistent with the example data: SBTF_data\input_data\radio_tower_locations\RTEastNorth_1group.xlsx
We recommend that each 'tower_group' has at least 100 training data locations and that towers are consistent in design among the groups.
3. Run train_model.ipynb, which will prompt the following user inputs:
- Training data location (e.g. SBTF_data\input_data\train_test_data\train_data.xlsx).
- Testing data location (e.g. SBTF_data\input_data\train_test_data\test_data.xlsx).
- Radio tower data location (e.g. SBTF_data\input_data\radio_tower_locations\RTEastNorth_1group.xlsx).
- Model save path: location you wish to save the trained models. Two models will be saved, one for each of the x and y axes.
Step 3 may take ~20 minutes depending on CPU speed.
- Time frequency (in mintues) you wish to group data. This will be study specific and depends on factors such as tag pulse interval and receiver scan interval.

#### Inference
Run predict.ipynb, which will prompt the following user inputs:
- Unlabelled reciever data. Make sure it is in the same format as the training/testing data (e.g. SBTF_data\input_data\train_test_data\test_data.xlsx), except it should exclude the POINT_X (longitude) and POINT_Y (latitude) columns.
- Radio tower data location (e.g. SBTF_data\input_data\radio_tower_locations\RTEastNorth_1group.xlsx).
- Model save path, same path as Step 3 of the model training step.
- Predictions save path, which is the location of export for the estimated locations of the reciever data.
- Time frequency (in mintues) you wish to group data. As per the model training step.