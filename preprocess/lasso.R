# Comments ----------------------------------------------------------------

## 1. The script has been updated to specify the interception variable and
##    runs as expected.

# -------------------------------------------------------------------------
# Installing/Loading Dependencies -----------------------------------------

## updating lock file
renv::snapshot()

packages = c('tidyverse','glue','glmnet')
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

fname = './data/preprocess/interception/int_pbp_preprocess.Rdata'
load(fname)

# -------------------------------------------------------------------------
# Building Design Matrix --------------------------------------------------

## setting seed
set.seed(1)

## building design matrix
X = model.matrix(object=interception ~ ., data=int_pbp_preprocess)[,-1]

## extracting interception
y = int_pbp_preprocess$interception

## creating CV plots
lasso = cv.glmnet(X, y, alpha=1)
plot(lasso)
coef = coef(lasso, s='lambda.min')

## creating feature and coefficient dataframes
feature_df = data.frame(features=rownames(coef)[-1],
                        coefficient=as.vector(coef)[-1])
glimpse(feature_df)

## sorting features
feature_df = arrange(feature_df, desc(abs(coefficient)))

## all features with nonzero coefficients
best_features = feature_df[feature_df$coefficient!=0,]
glimpse(best_features)

best_features = c('goal_to_go','drive_play_count','third_down',
                  'shotgun','posteam_timeouts_remaining','qb_scramble',
                  'yards_gained','weather','home_opening_kickoff',
                  'defteam_timeouts_remaining','roof','score_differential',
                  'yardline_100','ydstogo','start_hour','age',
                  'game_seconds_remaining','temp')

## creating the analysis chosen by LASSO
int_lasso = cbind(int_pbp_preprocess[,c('interception', best_features)])
glimpse(int_lasso)

# -------------------------------------------------------------------------
# Saving the Analysis Set Chosen By LASSO ---------------------------------

fname = './data/analysis/interception/lasso/int_lasso.Rdata'
save(int_lasso, file = fname)

# -------------------------------------------------------------------------