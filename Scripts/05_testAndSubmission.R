# ====================================================================================================== #
# Description
#
#   Test and submission
#
# Change log:
#   Ver   Date        Comment
#   1.0   28/03/23    Initial version
#
# ====================================================================================================== #
# ------------------------------------------------------------------------------------------------------ #
# LIBRARIES
# ------------------------------------------------------------------------------------------------------ #

library(data.table)
library(tidyverse)
library(magrittr)

library(tidymodels)

# ------------------------------------------------------------------------------------------------------ #
# IMPORT AND SOURCES
# ------------------------------------------------------------------------------------------------------ #

df_test <- fread("./Data/test.csv")

load("./Output/01_output.RData")
load("./Output/03_output.RData")
load("./Output/04a_tracking.RData")
load("./Output/04_output.RData")

# -- Custom metric
source("./Scripts/04b_customMetrics.R")

# ------------------------------------------------------------------------------------------------------ #
# PROGRAM
# ------------------------------------------------------------------------------------------------------ #

df_test_split <- testing(output_01$data_split)

best_wflow_id <- "recipe_2_xgboost"

cv_results_best <- 
  output_04$cv_results %>% 
  extract_workflow_set_result(best_wflow_id) %>% 
  select_best(metric = "rmsle")

# ---- Performance estimate on test set ----------------------------------------

performance_est <- 
  output_04$output_04$cv_results %>% 
  extract_workflow(
    best_wflow_id
  ) %>% 
  finalize_workflow(
    output_04$cv_results_best
  ) %>% 
  last_fit(
    split = output_01$data_split,
    metrics = output_04$my_metric_set
  )

# Performance
performance_est %>% 
  collect_metrics()

# Predictions
df_test_preds <- 
  performance_est %>% 
  collect_predictions()

df_test_preds %>% 
  ggplot(aes(x = cost, y = .pred)) +
  stat_bin_hex() +
  geom_abline(color = "blue")

# ---- Track experiment --------------------------------------------------------

# Track result for best_wflow_id

exp_results <- 
  tibble(
    "time" = lubridate::now(),
    "wflow_name" = best_wflow_id,
    "val.or.test" = "Test results"
  )

exp_results$recipe <- 
  best_wflow_id %>% 
  purrr::set_names() %>% 
  map( # map is unnecessary since length(best_wflow_id) == 1, but it returns a list, which is what we want
    ~ extract_preprocessor(
        x = output_04$cv_results,
        id = .x
      ) %>% 
      butcher::butcher() # reduce memory demand
  )
  
exp_results$model.spec <- 
  best_wflow_id %>% 
  purrr::set_names() %>% 
  map(
    ~ extract_spec_parsnip(
        x = output_04$cv_results,
        id = .x
      )
  )

exp_results$tuned.par <- 
  best_wflow_id %>% 
  purrr::set_names() %>% 
  map(
    ~ output_04$cv_results %>% 
      extract_workflow_set_result(.x) %>% 
      select_best(metric = "rmsle")
  )

exp_results$metrics <- 
  performance_est %>% 
  collect_metrics() %>% 
  mutate(
    wflow_id = best_wflow_id
  ) %>% 
  select(
    wflow_id,
    .metric,
    .estimate,
    .estimator
  ) %>% list() %>% 
  set_names(best_wflow_id)

# -- Append to tracking data
df_expTracking %<>% 
  dplyr::rows_append(
    y = exp_results
  )


# ---- Submission --------------------------------------------------------------

wflow_final_spec <- 
  output_04$cv_results %>% 
  extract_workflow(
    best_wflow_id
  ) %>% 
  finalize_workflow(
    cv_results_best
  )

wflow_final_fit <- 
  wflow_final_spec %>% 
  fit(output_01$df_train)

wflow_final_fit <- 
  wflow_final_spec %>% 
  fit(output_01$df_train)

subm_pred <- 
  wflow_final_fit %>% 
  predict(
    new_data = output_01$df_test
  )

df_subm <- 
  df_test %>% 
  cbind(subm_pred) %>% # swap for gam or ensemble
  select(
    id,
    "cost" = .pred
  ) 


# ==== EXPORT ------------------------------------------------------------------------------------------ 

df_subm %>% 
  fwrite(file = "./Output/05_submission.csv") 
