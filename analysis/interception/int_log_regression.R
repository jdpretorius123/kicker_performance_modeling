# Comments ----------------------------------------------------------------

## 1. The script has been updated to specify the interception variable and
##    runs as expected.

# -------------------------------------------------------------------------
# Installing/Loading Dependencies -----------------------------------------

# updating lock file
renv::snapshot()

packages = c('tidyverse', 'ROCR')
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
# Loading Play-by-Play Data -----------------------------------------------

fname = './data/analysis/interception/lasso/int_lasso.Rdata'
load(fname)

# -------------------------------------------------------------------------
# Creating Test Set -------------------------------------------------------

chosen_predictors = c('goal_to_go','drive_play_count','third_down','shotgun',
                      'posteam_timeouts_remaining','yards_gained',
                      'defteam_timeouts_remaining','score_differential',
                      'yardline_100','ydstogo','game_seconds_remaining',
                      'interception')

test_set = int_lasso %>%
  select(all_of(chosen_predictors))

# -------------------------------------------------------------------------
# Loading Trained Model ---------------------------------------------------
fname = './models/interception/trained/int_trained_model.rds'
int_trained_model = readRDS(file=fname)

# -------------------------------------------------------------------------
# Calculating the Predicted Probability of an Interception ----------------

pred_prob = predict(int_trained_model, newdata=test_set, type='response')
hist(x=pred_prob,
     main='Predicted Probabilities of an Interception',
     xlab='Predicted Probability',
     ylab='Frequency')
median_pred_prob = median(pred_prob)

# -------------------------------------------------------------------------
# Finding an Optimal Threshold --------------------------------------------

## creating a prediction object
pred_obj = ROCR::prediction(predictions=pred_prob,
                            labels=test_set$interception)

## F1-Score - the harmonic mean of precision and recall
f1_score = ROCR::performance(pred_obj, 'f')
f1_score_vals = f1_score@y.values[[1]]
f1_best_threshold = pred_obj@cutoffs[[1]][which.max(f1_score_vals)]
cat('Optimal Threshold for F1-Score:', f1_best_threshold, '\n')
cat('Max F1-Score:', f1_score_vals[which.max(f1_score_vals)], '\n')

## Youden's J Statistic: sensitivity + specificity - 1
## Youden's J denotes the point on the ROC curve farthest from the diagonal line
roc_performance = ROCR::performance(pred_obj, 'tpr', 'fpr')
youden_j_vals = roc_performance@y.values[[1]] - roc_performance@x.values[[1]]
youden_j_best_threshold = pred_obj@cutoffs[[1]][which.max(youden_j_vals)]
cat("Optimal Threshold for Youden's J Statistic:", youden_j_best_threshold, '\n')

## Precision-Recall Curve: optimal threshold depends on choice to prioritize
## precision or recall
## finding a threshold that gives a precision of at least 5%
pr_performance = ROCR::performance(pred_obj, 'prec', 'rec')
plot(pr_performance, main='Precision-Recall Curve')
pr_thresholds = pred_obj@cutoffs[[1]]
precision_values = pr_performance@y.values[[1]]

## finding the threshold that gives a precision of at least 5%
pr_best_threshold = pr_thresholds[which(precision_values >= 0.05)[1]]
cat('Threshold for Precision >= 5%:', pr_best_threshold, '\n')

# -------------------------------------------------------------------------
# Calculating Accuracy, Precision, Sn, and Sp -----------------------------

## calculating accuracy, precision, Sn, and Sp using F1-Score optimal
## threshold
pred_interception = if_else(pred_prob >= youden_j_best_threshold, 1, 0)
test_set$pred_interception = pred_interception

## coercing predicted and actual interception labels into factors
test_set$interception = as.factor(test_set$interception)
test_set$pred_interception = as.factor(test_set$pred_interception)

confusion_matrix = table(Actual=test_set$interception,
                         Predicted=test_set$pred_interception)

true_positive = as.integer(confusion_matrix[2,2])
false_negative = as.integer(confusion_matrix[2,1])
true_negative = as.integer(confusion_matrix[1,1])
false_positive = as.integer(confusion_matrix[1,2])

accuracy = (true_positive+true_negative)/sum(confusion_matrix)
precision = true_positive/(true_positive+false_positive)
sensitivity = true_positive/(true_positive+false_negative)
specificity = true_negative/(true_negative+false_positive)

cat('Accuracy:', round(accuracy, 4), '\n')
cat('Precision:', round(precision, 4), '\n')
cat('Sensitivity:', round(sensitivity, 4), '\n')
cat('Specificity:', round(specificity, 4), '\n')

# -------------------------------------------------------------------------
