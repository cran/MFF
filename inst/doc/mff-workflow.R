## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4
)

## ----bootstrap----------------------------------------------------------------
library(MFF)

set.seed(123)
boot_fit <- boot.train(
  target = "medv",
  data = MASS::Boston,
  ntest = 50,
  nvalid = 50,
  B = 20,
  seed = 123,
  parallel = FALSE
)

dim(boot_fit$pred_matrix_valid)
dim(boot_fit$pred_matrix_test)
boot_fit$metadata

## ----tune---------------------------------------------------------------------
tuned <- tune.mff(
  x = boot_fit$pred_matrix_valid,
  y = boot_fit$y_valid,
  max_c = 4,
  mff.method = "kmeans",
  eval.method = "RMSE",
  nstart = 20,
  seed = 123,
  parallel = FALSE,
  logging = FALSE
)

tuned$best_params
tuned$best_cluster
tuned$best_scores

## ----predict------------------------------------------------------------------
test_prediction <- predict(
  tuned,
  pred_matrix = boot_fit$pred_matrix_test,
  type = "best"
)

head(test_prediction$mff_preds)
test_prediction$mff_weights

## ----evaluate-----------------------------------------------------------------
evaluate(test_prediction$mff_preds, boot_fit$y_test)

## ----external-inputs, eval = FALSE--------------------------------------------
# tuned <- tune.mff(
#   x = validation_predictions,
#   y = validation_response,
#   max_c = 4,
#   mff.method = "gk",
#   eval.method = "RMSE"
# )
# 
# final_prediction <- predict(
#   tuned,
#   pred_matrix = test_predictions,
#   type = "best"
# )

