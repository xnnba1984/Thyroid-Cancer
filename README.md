# Improving The Diagnosis of Thyroid Cancer by Machine Learning and Clinical Data
This repository contains the code generating the result in the paper 'Improving The Diagnosis of Thyroid Cancer by Machine Learning and Clinical Data'. Please check our [preprint](https://arxiv.org/abs/2203.15804) for details.

## Code Structure

### 1. data_preprocess.R

The data clean, preprocess, and format transformation.

### 2. model_selection.R

The descriptive statistics of the dataset; the model performance of logistic regression, random forest, GBM, LDA, and SVM, measured by 10-fold cross-validation.

### 3. boostrap_logistic.R, boostrap_randomforest.R, boostrap_gbm.R, boostrap_lda.R, boostrap_svm.R

The model performance uncertainty of logistic regression, random forest, GBM, LDA, and SVM, measured by boostrap analysis.

### 4. permutation_importance_logistic.R, permutation_importance_randomforest.R, permutation_importance_gbm.R, permutation_importance_lda.R, permutation_importance_svm.R

The normalized permutation predictor importance calculated under logistic regression, random forest, GBM, LDA, and SVM.

### 5. expert_compare.R

The comparison of five prediction measurements between expert assessment and random forest model.

### 6. figure.R

All the R code that creates the figures in the paper.
