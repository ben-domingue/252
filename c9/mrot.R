
#' Perform k-fold cross-validation to get out-of-sample predictions (Robust Version)
#'
#' This function takes two fitted glmer models, which may have different predictors,
#' performs k-fold cross-validation, and returns a data frame with out-of-sample
#' predictions for each model. It correctly handles cases where models use
#' different sets of variables.
#'
#' @param m1 A fitted model object of class 'glmerMod'.
#' @param m2 A fitted model object of class 'glmerMod'.
#' @param k The number of folds for cross-validation. Defaults to 10.
#' @param seed An integer for the random number generator to ensure reproducible folds.
#'   Defaults to 123.
#' @param re.form_arg The formula for random effects to use for prediction, passed
#'   to `lme4::predict`. Defaults to `NA` for population-level predictions.
#' @param ... Additional arguments passed on to `glmer` during refitting.
#'
#' @return A data frame containing all variables used by either model, a column
#'   for the fold assignment ('fold'), and the out-of-sample predictions from
#'   m1 and m2 ('pred_m1' and 'pred_m2'). The output is self-contained and
#'   guaranteed to be row-aligned.
#'
#' @examples
#' if (require(lme4)) {
#'   # --- Example 1: Original use case ---
#'   m1 <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
#'               data = cbpp, family = binomial)
#'   m2 <- glmer(cbind(incidence, size - incidence) ~ period + (1 + period | herd),
#'               data = cbpp, family = binomial,
#'               control = glmerControl(optimizer = "bobyqa"))
#'
#'   cv_preds <- cross_validate_predictions_robust(m1, m2, k = 5, seed = 42)
#'   head(cv_preds)
#'
#'   # --- Example 2: Demonstrating robustness to different predictors ---
#'   # Create a new predictor variable
#'   cbpp$obs_id <- 1:nrow(cbpp)
#'
#'   # Model 1 uses 'period'
#'   m1_diff <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
#'                    data = cbpp, family = binomial)
#'
#'   # Model 2 uses 'obs_id' instead of 'period'
#'   m2_diff <- glmer(cbind(incidence, size - incidence) ~ obs_id + (1 | herd),
#'                    data = cbpp, family = binomial)
#'
#'   # The old function would have failed here. The new one works.
#'   cv_preds_diff <- cross_validate_predictions_robust(m1_diff, m2_diff, k = 5, seed = 42)
#'   
#'   # Note that the output contains 'period', 'obs_id', and all other necessary variables
#'   head(cv_preds_diff)
#'
#'   # Calculate RMSE to compare
#'   cv_preds_diff$observed_prop <- cv_preds_diff$`cbind(incidence, size - incidence)`.incidence /
#'     (cv_preds_diff$`cbind(incidence, size - incidence)`.incidence +
#'      cv_preds_diff$`cbind(incidence, size - incidence)`.`size - incidence`)
#'
#'   rmse_m1 <- sqrt(mean((cv_preds_diff$observed_prop - cv_preds_diff$pred_m1)^2))
#'   rmse_m2 <- sqrt(mean((cv_preds_diff$observed_prop - cv_preds_diff$pred_m2)^2))
#'
#'   cat("\n--- Comparison of Models with Different Predictors ---\n")
#'   cat("RMSE for Model 1 (using 'period'):", rmse_m1, "\n")
#'   cat("RMSE for Model 2 (using 'obs_id'):", rmse_m2, "\n")
#' }
#'
cross_validate_predictions_robust <- function(m1, m2, k = 10, seed = 123, re.form_arg = NA, ...) {

  # --- Input Validation ---
  if (!inherits(m1, "glmerMod") || !inherits(m2, "glmerMod")) {
    stop("Both m1 and m2 must be 'glmerMod' objects from the lme4 package.")
  }

  # --- 1. Create a Master Dataset for CV ---
  # Extract the model frames, which are the exact datasets used for fitting
  data1 <- m1@frame
  data2 <- m2@frame

  # Sanity check: Ensure models were fit on the same number of observations.
  # This is a crucial check for data consistency.
  if (nrow(data1) != nrow(data2)) {
    stop(paste(
      "Models were not fit on the same number of data points.",
      "nrow(m1@frame) =", nrow(data1), "but nrow(m2@frame) =", nrow(data2),
      "This can happen if predictors have different patterns of missing values."
    ))
  }
  n_obs <- nrow(data1)

  # Find columns in m2's data that are not in m1's data
  unique_cols_m2 <- setdiff(names(data2), names(data1))

  # Combine the data from m1 with the unique columns from m2.
  # This creates a 'master' dataset containing all variables for both models.
  if (length(unique_cols_m2) > 0) {
    model_data <- cbind(data1, data2[, unique_cols_m2, drop = FALSE])
  } else {
    model_data <- data1
  }

  # --- 2. Create Folds ---
  set.seed(seed)
  folds <- cut(sample(seq_len(n_obs)), breaks = k, labels = FALSE)
  model_data$fold <- folds

  # --- 3. Initialize Storage for Predictions ---
  # Use a column in the data frame to store results, which is safer.
  model_data$pred_m1 <- numeric(n_obs)
  model_data$pred_m2 <- numeric(n_obs)

  # --- 4. K-Fold Cross-Validation Loop ---
  for (i in seq_len(k)) {
    cat(sprintf("Processing fold %d of %d...\n", i, k))

    # Identify indices for the current fold
    test_indices <- which(folds == i)

    # Create training and test data from our master dataset
    train_data <- model_data[-test_indices, ]
    test_data <- model_data[test_indices, ]

    # Refit models on the training data. update() will find all needed variables.
    m1_refit <- update(m1, data = train_data)
    m2_refit <- update(m2, data = train_data)

    # Generate and store predictions on the test data
    model_data$pred_m1[test_indices] <- predict(m1_refit, newdata = test_data, type = "response", re.form = re.form_arg)
    model_data$pred_m2[test_indices] <- predict(m2_refit, newdata = test_data, type = "response", re.form = re.form_arg)
  }

  # --- 5. Return Final, Aligned Data ---
  cat("Cross-validation complete.\n")

  # The 'model_data' data frame now contains everything needed:
  # - All predictors from both models
  # - The response variable
  # - The fold assignments
  # - The out-of-sample predictions for each model
  return(model_data)
}
rms<-function(x,y) sqrt(mean((x-y)^2))
library(lme4)
library(dplyr)
library(ggplot2)
library(stringr)

########################################################################
##First let's ensure this function is not doing something too wacky
mu<-rnorm(100)
x<-rnorm(5000)
gr<-sample(1:length(mu),5000,replace=TRUE)
k<-x+mu[gr]
p<-1/(1+exp(-k))
y<-rbinom(length(x),1,p)
df<-data.frame(x=x,y=y,gr=gr,x2=x^2)
m1<-glmer(y~x+(1|gr),df,family='binomial')
m2<-glmer(y~x+x2+(1|gr),df,family='binomial')
cv<-cross_validate_predictions_robust(m1,m2)
rms(cv$y,cv$pred_m1)
rms(cv$y,cv$pred_m2)
library(imv)
imv.binary(cv$y,cv$pred_m1,cv$pred_m2) ##nice...

########################################################################
mrot_rt_trial_no_retest<-readRDS("mrot_rt_trial_no_retest.rds") ##see 252/c9 slides
# 1. Prep the data: Extract angle and filter for correct trials
df <- mrot_rt_trial_no_retest %>%
    mutate(
                                        # Extract the angle from the item name (e.g., "shape_120" -> 120)
        angle = as.numeric(str_extract(item, "\\d+")),
                                        # Optional: extract stimulus type to see 2D vs 3D differences
        stimulus = str_extract(item, "^[a-z]+") 
  ) %>%
                                        # Keep only correct responses with valid reaction times
    filter( is.finite(log_rt), !is.na(angle))

##models
m1<-glmer(correct~angle+(1|user_id),df,family='binomial')
df$item2<-paste('angle',df$angle,sep='')
m2<-glmer(correct~item2+(1|user_id),df,family='binomial')

cv<-cross_validate_predictions_robust(m1,m2)
rms(cv$correct,cv$pred_m1)
rms(cv$correct,cv$pred_m2)

library(imv)
imv.binary(cv$correct,cv$pred_m1,cv$pred_m2)

df$rad<-df$angle*pi/180
df$rad2<-df$rad^2
m1a<-glmer(correct~rad+(1|user_id),df,family='binomial')
m2a<-glmer(correct~rad+rad2+(1|user_id),df,family='binomial')
cva<-cross_validate_predictions_robust(m1a,m2a,k=5)
library(imv)
imv.binary(cva$correct,cva$pred_m1,cva$pred_m2)
