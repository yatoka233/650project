# 650project
## 1. Descriptive analysis of AGE4, TOTMMSE4, TOTIADL4, TOTADL4, EE46
* As we can not impute outcome variable, we first check the distribution of all 4 continuous variables and 1 categorical variable(predictor of interest).
* From boxplot, density plot, hisogram, and KS test we can see that for complete data and incomplete data, the distribution of continuous variables are significantly different. Therefore it is necessary to conduct imputation for covariates.

## 2. Imputation
* We tried 4 kinds of imputation for continuous variables (KNN, MICE, MEAN, MEDIAN).
* We use mode to impute categorical variables.
* We drop missing outcome variables.
