# Comments ----------------------------------------------------------------

## 1. The script has been updated to specify the interception variable and
##    runs as expected.

# -------------------------------------------------------------------------
# Installing/Loading Dependencies -----------------------------------------

## updating lock file
renv::snapshot()

packages = c('tidyverse', 'caret', 'xgboost', 'pROC', 'ROCR')
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
int_lasso = as_tibble(int_lasso)

# -------------------------------------------------------------------------
# Creating Test Set -------------------------------------------------------

chosen_predictors = c('goal_to_go','drive_play_count','third_down','shotgun',
                      'posteam_timeouts_remaining','yards_gained',
                      'defteam_timeouts_remaining','score_differential',
                      'yardline_100','ydstogo','game_seconds_remaining',
                      'interception')

xgb_data = int_lasso %>%
  select(all_of(chosen_predictors))
glimpse(xgb_data)

# -------------------------------------------------------------------------
# Preparing the Data for XGBoost Model ------------------------------------

## identifying the factors that need to be one-hot encoded
factors = sapply(xgb_data, is.factor)
factors_to_encode = names(xgb_data)[factors]

## creating a dummy variable model for one-hot encoding
encode_model = dummyVars(paste('~', paste(factors_to_encode, collapse = ' + ')),
                         data = xgb_data)

## applying one-hot encoding to the data
factors_one_hot = predict(encode_model, newdata = xgb_data)

## combining numeric columns with the new one-hot encoded columns
numeric_cols = xgb_data[,!factors]
encoded_xgb_data = cbind(numeric_cols, factors_one_hot)

set.seed(1)

## splitting the data into training and testing sets
sample_size = floor(0.8 * nrow(encoded_xgb_data))
sample_indices = sample(nrow(encoded_xgb_data),
                        size = sample_size,
                        replace = FALSE)

train = encoded_xgb_data[sample_indices,]
test = encoded_xgb_data[-sample_indices,]

X_train = train %>%
  dplyr::select(!interception)
y_train = train$interception

X_test = test %>%
  dplyr::select(!interception)
y_test = test$interception

# -------------------------------------------------------------------------
# Training XGBoost Model --------------------------------------------------

## converting the data into DMarix format for the XGBoost model
dtrain = xgb.DMatrix(data = as.matrix(X_train), label = y_train)

## defining model parameters
params = list(objective = 'binary:logistic',
              eta = 0.1,
              max_depth = 6,
              subsample = 0.8,
              colsample_bytree = 0.8)

## running cross-validation to find the optimal number of boosting rounds
cv = xgb.cv(params = params,
            data = dtrain,
            nrounds = 100,
            nfold = 5,
            early_stopping_rounds = 10,
            eval_metric = 'auc',
            verbose = 0,
            seed = 1)
best_nrounds = cv$best_iteration

## training the final XGBoost model using the optimal number of boosting rounds
xgb = xgb.train(params = params, data = dtrain, nrounds = best_nrounds)

# -------------------------------------------------------------------------
# Testing XGBoost Model ---------------------------------------------------

## converting the test data into an xGBoost DMatrix
dtest = xgb.DMatrix(data = as.matrix(X_test), label = y_test)

# -------------------------------------------------------------------------
# Evaluating XGBoost Model Performance on Test Data -----------------------

predicted_probs = predict(xgb, dtest)
actual_values = getinfo(dtest, 'label')

## calculating the AUC
auc = auc(actual_values, predicted_probs)

## creating a prediction object
pred_obj = ROCR::prediction(predictions=predicted_probs,
                            labels=actual_values)

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
pred_interception = if_else(predicted_probs >= f1_best_threshold, 1, 0)
test$pred_interception = pred_interception

## coercing predicted and actual interception labels into factors
test$interception = as.factor(test$interception)
test$pred_interception = as.factor(test$pred_interception)

confusion_matrix = table(Actual=test$interception,
                         Predicted=test$pred_interception)

true_positive = as.integer(confusion_matrix[2,2])
false_negative = as.integer(confusion_matrix[2,1])
true_negative = as.integer(confusion_matrix[1,1])
false_positive = as.integer(confusion_matrix[1,2])

accuracy = (true_positive+true_negative)/sum(confusion_matrix)
precision = true_positive/(true_positive+false_positive)
sensitivity = true_positive/(true_positive+false_negative)
specificity = true_negative/(true_negative+false_positive)

## compiling and displaying the results of the XGBoost model performance
xgb_performance = data.frame(
  Metric = c('Accuracy', 'Precision', 'Sensitivity', 'Specificity'),
  Value = c(accuracy, precision, sensitivity, specificity)
) %>%
  mutate(Value = round(Value, 2))
xgb_performance

# -------------------------------------------------------------------------
# Evaluating the XGBoost Model on the Entire Dataset  ---------------------

encoded_data = encoded_xgb_data %>%
  select(!interception)
dfull_data = xgb.DMatrix(data = as.matrix(encoded_data))
xgb_data$prob_interception = predict(xgb, dfull_data)
glimpse(xgb_data)

## calculating accuracy, precision, Sn, and Sp using F1-Score optimal
## threshold
pred_interception = if_else(xgb_data$prob_interception >= f1_best_threshold, 1, 0)
xgb_data$pred_interception = pred_interception

## coercing predicted and actual interception labels into factors
xgb_data$interception = as.factor(xgb_data$interception)
xgb_data$pred_interception = as.factor(xgb_data$pred_interception)

confusion_matrix = table(Actual=xgb_data$interception,
                         Predicted=xgb_data$pred_interception)

true_positive = as.integer(confusion_matrix[2,2])
false_negative = as.integer(confusion_matrix[2,1])
true_negative = as.integer(confusion_matrix[1,1])
false_positive = as.integer(confusion_matrix[1,2])

accuracy = (true_positive+true_negative)/sum(confusion_matrix)
precision = true_positive/(true_positive+false_positive)
sensitivity = true_positive/(true_positive+false_negative)
specificity = true_negative/(true_negative+false_positive)

## compiling and displaying the results of the XGBoost model performance
xgb_perf_full_data = data.frame(
  Metric = c('Accuracy', 'Precision', 'Sensitivity', 'Specificity'),
  Value = c(accuracy, precision, sensitivity, specificity)
) %>%
  mutate(Value = round(Value, 2))
xgb_perf_full_data

# -------------------------------------------------------------------------
# Playing Around with Extra Metrics for Another Project -------------------

fname = './data/cleaned/interception/int_pbp_clean.Rdata'
load(fname)
glimpse(int_pbp_clean)

select_vars = c('passer_id','display_name','season','week')
pbp_select_vars = int_pbp_clean %>%
  dplyr::select(all_of(select_vars))

xgb_data = cbind(xgb_data, pbp_select_vars)
glimpse(xgb_data)

qb_evaluation = xgb_data %>%
  group_by(passer_id, display_name) %>%
  summarise(actual_ints = sum(interception, na.rm = TRUE),
            predicted_ints = sum(prob_interception, na.rm = TRUE),
            pass_attempts = n(),
            ioe = actual_ints - predicted_ints,
            actual_ir = (actual_ints/pass_attempts)*100,
            predicted_ir = (predicted_ints/pass_attempts)*100,
            iroe = actual_ir - predicted_ir,
            .groups = 'drop') %>%
  filter(pass_attempts >= quantile(pass_attempts, prob = 0.25)) %>%
  left_join(y = int_pbp_clean %>% 
              dplyr::select(passer_id, age) %>%
              distinct() %>%
              group_by(passer_id) %>%
              mutate(seasons = last(age) - first(age)) %>%
              ungroup() %>%
              dplyr::select(passer_id, seasons) %>%
              distinct(),
            by = 'passer_id') 
glimpse(qb_evaluation)

# -------------------------------------------------------------------------
