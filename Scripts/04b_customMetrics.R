# ====================================================================================================== #
# Description
#
#   Custom Metric: Yardstick implementation of RMSLE
#   See https://www.tidymodels.org/learn/develop/metrics/
#
# Change log:
#   Ver   Date        Comment
#   1.0   26/03/23    Initial version
#
# ====================================================================================================== #
# ------------------------------------------------------------------------------------------------------ #
# LIBRARIES
# ------------------------------------------------------------------------------------------------------ #

library(tidymodels)
library(rlang)

# ------------------------------------------------------------------------------------------------------ #
# PROGRAM
# ------------------------------------------------------------------------------------------------------ #

# -- Vector version

rmsle_vec <- function(truth, estimate, na_rm = TRUE, ...) {
  
  rmsle_impl <- function(truth, estimate) {
    
    sqrt(mean((log1p(truth) - log1p(estimate)) ^ 2))
    
  }
  
  metric_vec_template(
    metric_impl = rmsle_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
  
}

# -- Dataframe implementation

rmsle <- function(data, ...) {
  
  UseMethod("rmsle")
  
}

rmsle <- 
  new_numeric_metric(
    rmsle, 
    direction = "minimize"
  )

rmsle.data.frame <- function(data, truth, estimate, na_rm = TRUE, ...) {
  
  metric_summarizer(
    metric_nm = "rmsle",
    metric_fn = rmsle_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate), 
    na_rm = na_rm,
    ...
  )
  
}

# -- Unit test
#    https://scikit-learn.org/stable/modules/model_evaluation.html#mean-squared-log-error

if (FALSE) { # avoid running test when sourcing
  
  y_true <- c(3, 5, 2.5, 7)
  y_pred <- c(2.5, 5, 4, 8)
  
  rmsle_vec(y_true, y_pred)
  
}


