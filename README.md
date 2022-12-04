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
* We tried mode imputation for categorical variables.
* We finally use MICE for imputation. (PMM (Predictive Mean Matching) – For numeric variables, logreg(Logistic Regression) – For Binary Variables( with 2 levels), polyreg(Bayesian polytomous regression) – For Factor Variables (>= 2 levels), Proportional odds model (ordered, >= 2 levels))
* We drop missing outcome variables.

## 4. Descriptive analysis after imputation

## 5. Data restructure (make groups for Age and Grade) and correlation analysis (IADL and ADL)

## 6. Bivariate analysis of outcome and covariate (Drop Nkids)

## 7. Outcome transform

## 8. Main model diagnosis

## 9. Significance analysis (sequential model)

## 10. Interaction exploration

## 11. Interpretation


