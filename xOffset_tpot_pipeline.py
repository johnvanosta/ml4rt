import numpy as np
import pandas as pd
from sklearn.ensemble import ExtraTreesRegressor
from sklearn.linear_model import LassoLarsCV, SGDRegressor
from sklearn.model_selection import train_test_split
from sklearn.pipeline import make_pipeline, make_union
from tpot.builtins import StackingEstimator
from tpot.export_utils import set_param_recursive

# NOTE: Make sure that the outcome column is labeled 'target' in the data file
tpot_data = pd.read_csv('PATH/TO/DATA/FILE', sep='COLUMN_SEPARATOR', dtype=np.float64)
features = tpot_data.drop('target', axis=1)
training_features, testing_features, training_target, testing_target = \
            train_test_split(features, tpot_data['target'], random_state=38)

# Average CV score on the training set was: -233.92694733558378
exported_pipeline = make_pipeline(
    StackingEstimator(estimator=ExtraTreesRegressor(bootstrap=False, max_features=0.55, min_samples_leaf=17, min_samples_split=15, n_estimators=100)),
    StackingEstimator(estimator=LassoLarsCV(normalize=True)),
    SGDRegressor(alpha=0.0, eta0=0.01, fit_intercept=True, l1_ratio=0.0, learning_rate="invscaling", loss="huber", penalty="elasticnet", power_t=0.5)
)
# Fix random state for all the steps in exported pipeline
set_param_recursive(exported_pipeline.steps, 'random_state', 38)

exported_pipeline.fit(training_features, training_target)
results = exported_pipeline.predict(testing_features)
