# XGBoost in Insurance 2017
Data and Code to reproduce results for my talk at Paris: `R in Insurance 2017 Conference`

## Non life pricing: empirical comparison of classical GLM with tree based Gradient Boosted Models

To reproduce the results, please:
- clone the repository
- set your working directory to the cloned depository
- Inside the `code folder` run:
  - `00_init_*`  to initiate the workspace correctly. 
  - `02_train_*`  to execute parameter tuning and save xgboost models.
  - `03_scoring`  to execute cross-validation and obtain results, on both GAM and xgboost.

Further, `01_preprocess_*.R` code to prepare the dataset is provided.
Note: xgboost mdoels are not provided directly since they exceed the size limit of GitHub. Feel free to reach out and I will provide them privately if pre-tuning is too expensive.

Thank you!



