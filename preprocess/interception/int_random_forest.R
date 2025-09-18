# Comments ----------------------------------------------------------------

## 1. The script has been updated to specify the interception variable and
##    runs as expected.

# -------------------------------------------------------------------------
# Installing/Loading Dependencies -----------------------------------------

## updating lock file
renv::snapshot()

packages = c('tidyverse','glue','ranger')
check_packages = function(packages) {
  for (p in packages) {
    if (!require(p, character.only=TRUE)) {
      install.packages(p)
    }
    library(p, character.only=TRUE)
  }
}

check_packages(packages)

# -------------------------------------------------------------------------
# Clearing Memory ---------------------------------------------------------

rm(list=ls())

# -------------------------------------------------------------------------
# Loading Preprocessed Data -----------------------------------------------

fname='./data/preprocess/interception/int_pbp_preprocess.Rdata'
load(fname)

# -------------------------------------------------------------------------
# Variable Selection with Random Forest -----------------------------------

## setting a seed for reproducibility
set.seed(1)

## building random forest model
rf_model = ranger(interception ~ .,
                  data=int_pbp_preprocess,
                  num.trees=500,
                  importance='impurity')

## extracting feature importance scores
importance_scores = importance(rf_model)

## ranking predictors by importance
feature_df = data.frame(features=names(importance_scores),
                        importance=importance_scores)
glimpse(feature_df)

## sorting the features
feature_df = arrange(feature_df, desc(importance))
glimpse(feature_df)

## extracting the best features
best_features = feature_df[1:23,]
best_features

## creating the feature dataset chosen by random forest
int_rf = cbind(int_pbp_preprocess['interception'],
                        int_pbp_preprocess[,best_features$features])
glimpse(int_rf)

# -------------------------------------------------------------------------
# Saving the Analysis Set Chosen By Random Forest -------------------------

fname = './data/analysis/interception/random_forest/int_rf.Rdata'
save(int_rf, file=fname)

# -------------------------------------------------------------------------
