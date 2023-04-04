# Kaggle_Playground_S3E11

We will follow the same process as E10 using tidymodels. Specifically, we will create a model pipeline with workflow_sets, which facilitates structured experiment tracking and easy preparation of the test in accordance with feature engineering. 

However, this time we will conduct expreiment tracking according to the following plan: 

1. Comparison of feature engineering steps: Create a base recipe from which a number of recipes will be created by adding or tweaking a single feature engineering step. This allows to easily compare each feature engineering step by using resampling. The choice of model is not important, although a model class, which is known for performing well for the problemt at hand, should be used (xgboost is a solid choice for most cases). Use reasonable parameter values (default values usually suffice), since we are not interested in tuning hyperparameters, only comparing feature engineering steps. 

2. Once we have found the most competitive recipes, it is time to compare model specifications and tune hyperparameters. If fitting takes a long time, due to large amounts of data etc., consider using only one recipe for the comparison of model specifications. 

3. Create an ensemble based on the set of most competitive recipes and model specifications

3. Track the validation and test results for all models tried.

## Expriment tracking 
The tracking itself is carried out using a tibble with list cols, to which each run is appended. We track the following information for each run:

* Recipe
* Model spec including estimated parameters (from best model if tuning was used)
* Metrics and metric values
* Are the metric values based on validation data or test data

## Tidymodels notes
A workflow_set creates a cartesian product of recipes and model specs. Sometimes, specific recipes only work with specific model specs and vice versa. In this case, simply create another workflow_set with these recipes and specs. Combine all workflow_sets in the end in order to compare models. 

Next time, lets try stacking in the tidymodels framework! Ensembling many competitive models turned out to be the winning solution to the competition. 

## Note on the competition metric

* https://www.kaggle.com/competitions/playground-series-s3e11/discussion/396295
* https://www.kaggle.com/competitions/playground-series-s3e11/discussion/396240

