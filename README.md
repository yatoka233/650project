# 650project
## 1. Data description
* 1682 observations and 20 variables (including ID).
* Outcome variable: CES-D total (`cesdtot4`), a measure of depression, with higher numbers representing higher levels of depression.
* Primary Predictors of interest:
  * Instrumental activities of daily living (`totiadl4`): (sum of) Instrumental activities of daily living are Driving, Preparing Meals, Doing Housework, Shopping, Managing Finances, Managing Medication, Using the Telephone—higher score reflects lesser ability to do more activities.
  * Religiosity (`ee46`): How religious are you?  (1=very religious, …, 4=not religious at all)
* 263 rows containing missing values.

## 2. Descriptive analysis of `AGE4`, `TOTMMSE4`, `TOTIADL4`, `TOTADL4`, `EE46`
* As we can not impute outcome variable, we first check the distribution of all 4 continuous variables and 1 categorical variable (predictor of interest).
* From boxplot, density plot, hisogram, and KS test we can see that for complete data and incomplete data, the distribution of continuous variables are significantly different. Therefore it is necessary to conduct imputation for covariates.

## 3. Imputation
* We tried 4 kinds of imputation for continuous variables (**KNN**, **MICE**, **MEAN**, **MEDIAN**).
* We use mode to impute categorical variables.
* We drop missing outcome variables.
