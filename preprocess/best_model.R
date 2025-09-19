# Comments ----------------------------------------------------------------

## 1. The script has been updated to specify the interception variable and
##    runs as expected.

# -------------------------------------------------------------------------
# Installing/Loading Dependencies -----------------------------------------

## updating lock file
renv::snapshot()

packages = c('tidyverse','glue','caret','car')
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
# Loading the Feature Sets Chosen by Random Forest and LASSO --------------

fname = './data/analysis/interception/random_forest/int_rf.Rdata'
load(fname)
glimpse(int_rf)

fname = './data/analysis/interception/lasso/int_lasso.Rdata'
load(fname)
glimpse(int_lasso)

# -------------------------------------------------------------------------
# Checking Features Chosen By Both RF and LASSO ---------------------------

common_features = intersect(colnames(int_lasso),
                            colnames(int_rf))
common_features

# -------------------------------------------------------------------------
# Checking Distinct Features Chosen By RF and LASSO -----------------------

rf_dist_feat = setdiff(colnames(int_rf),
                       colnames(int_lasso))
rf_dist_feat

lasso_dist_feat = setdiff(colnames(int_lasso),
                          colnames(int_rf))
lasso_dist_feat

# -------------------------------------------------------------------------
# Building the Formula for each Model -------------------------------------

## defining the formulas
rf_features = colnames(int_rf)
rf_formula = as.formula(paste('interception ~',
                              paste(rf_features[rf_features!='interception'],
                                    collapse = ' + ')))

lasso_features = colnames(int_lasso)
lasso_formula = as.formula(paste('interception ~',
                                 paste(lasso_features[lasso_features!='interception'],
                                       collapse =  ' + ')))

## creating both a temporary data set and a formula with all the features
## selected by random forest and LASSO
all_feat_analysis_set = cbind(int_rf[,..common_features],
                              int_rf[,..rf_dist_feat],
                              int_lasso[,..lasso_dist_feat])
glimpse(all_feat_analysis_set)

all_features = colnames(all_feat_analysis_set)
all_feat_formula = as.formula(paste('interception ~',
                                    paste(all_features[all_features!='interception'],
                                          collapse=' + ')))

# -------------------------------------------------------------------------
# Fitting the Models ------------------------------------------------------

## creating temporary RF data set
rf_temp = int_rf %>%
  mutate(interception = factor(interception, levels=c(0,1), labels=c('No','Yes')))
ctrl = trainControl(method = 'cv',
                    number = 5,
                    classProbs = TRUE,
                    summaryFunction = twoClassSummary,
                    sampling = 'smote')
rf_model = train(rf_formula,
                 data=rf_temp,
                 method = 'glm',
                 trControl = ctrl,
                 metric = 'ROC')

## creating temporary LASSO data set
lasso_temp = int_lasso %>%
  mutate(interception = factor(interception, levels=c(0,1), labels=c('No','Yes')))
lasso_model = train(lasso_formula,
                    data = lasso_temp, 
                    method = 'glm', 
                    trControl = ctrl,
                    metric = 'ROC')

## creating temporary All Feature data set
all_feat_temp = all_feat_analysis_set %>%
  mutate(interception = factor(interception, levels=c(0,1), labels=c('No','Yes')))
all_feat_model = train(all_feat_formula,
                       data = all_feat_temp, 
                       method = 'glm',
                       trControl = ctrl,
                       metric='ROC')

# -------------------------------------------------------------------------
# Comparing Model Performance ---------------------------------------------

model_performance = data.frame(Model = c('RF','LASSO','Both'),
                               ROC = c(rf_model$results$ROC,
                                       lasso_model$results$ROC,
                                       all_feat_model$results$ROC))
model_performance

# -------------------------------------------------------------------------
# Using AIC and BIC to Determine the Best Model ---------------------------

## 95% CI: Estimate +/- 1.96*SE

## building the model
fit = glm(lasso_formula, data=lasso_temp, family='binomial')

## using AIC to find the best model
model_aic = stats::step(fit, direction='both', trace=0)
summary(model_aic)
formula(model_aic)

## using BIC to find the best model
row_count = nrow(lasso_temp)
model_bic = stats::step(fit, direction='both', k=log(row_count), trace=0)
summary(model_bic)
formula(model_bic)

## listing the best predictors
chosen_predictors = c('goal_to_go','drive_play_count','third_down','shotgun',
                      'posteam_timeouts_remaining','yards_gained',
                      'defteam_timeouts_remaining','score_differential',
                      'yardline_100','ydstogo','game_seconds_remaining')

# -------------------------------------------------------------------------
# Building the Best Model -------------------------------------------------

formula = as.formula(paste('interception ~',
                           paste(chosen_predictors, collapse=' + ')))

colnames(lasso_temp)[names(lasso_temp)=='y']='interception'
int_trained_model = glm(formula, data=lasso_temp, family='binomial')
summary(int_trained_model)

# -------------------------------------------------------------------------
# Checking Model Assumptions ----------------------------------------------

## linearity and homoscedasticity
## linearity: relationship between predictors and response is linear
fitted = predict(int_trained_model, type='response')
residuals = residuals(int_trained_model, type='deviance')
resid_df = data.frame(fitted, residuals)
resid_plot = ggplot(data=resid_df,
                    mapping=aes(x=fitted,
                                y=residuals)) +
  geom_point(alpha=0.6) +
  geom_smooth(method='loess', se=FALSE, color='blue', linetype='dashed') +
  geom_hline(yintercept=0, linetype='dashed', color='red') +
  labs(title='Residual Plot',
       x='Fitted Values',
       y='Deviance Residuals') +
  theme_minimal()

## normality of residuals
normality_plot = ggplot(data=resid_df,
                        mapping=aes(sample=residuals)) +
  stat_qq(alpha=0.6) +
  stat_qq_line(color='blue', linetype='dashed') +
  labs(title='Normal Q-Q Plot',
       subtitle='Normality of Deviance Residuals',
       x='Theoretical Quantiles',
       y='Sample Quantiles') +
  theme_minimal()

## standardized residuals vs. leverage
## measuring the influence of each observation through Cook's distance
## large circles that are far away from the rest of the circles indicate 
## observations that exert large influence
influencePlot(int_trained_model)

## multicollinearity
## a VIF value greater than 5 or 10 suggests that multicollinearity may be
## present
vif_values = vif(int_trained_model)
vif_values
hist(x=vif_values,
     main='Histogram of VIF Values',
     xlab='VIF Value',
     col='lightblue',
     border='black')

# -------------------------------------------------------------------------
# Saving Trained Model ----------------------------------------------------

fname = './models/interception/trained/int_trained_model.rds'
saveRDS(int_trained_model, file=fname)

# -------------------------------------------------------------------------
