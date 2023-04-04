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

df_train_comb <- 
  output_01$df_train %>% 
  rows_append(
    y = output_01$df_train_org
  )

df_train_split <- training(output_01$data_split)

# using the combined data yields much better results
df_train_split_comb <- 
  df_train_split %>% 
  rows_append(
    y = output_01$df_train_org
  )

df_test_split <- testing(output_01$data_split)

cost_log <- TRUE # do we use rmse as loss? Then we log1p transform
if (cost_log) { # we cannot transform predictions in tune_grid, so this is easier than step_mutate(cost = log1p(cost), skip = TRUE)
  
  df_train_split_comb[
    ,
    cost := log1p(cost)
  ]
  
  df_train_comb[
    ,
    cost := log1p(cost)
  ]
  
}


# ---- Performance estimate on test set ----------------------------------------

best_wflow_id <- "recipe_play_lightgbm"

cv_results_best <- 
  output_04$cv_results %>% 
  extract_workflow_set_result(best_wflow_id) %>% 
  select_best(metric = "rmse")

comb <- TRUE

if (comb) { # last_fit uses the training data defined by data_split, which is the uncombined data. Therefore, we have do it "manually".
            # or maybe we dont ... I get the exact same .estimate, so maybe last_fit uses the data supplied to the recipe
  
  last_fit <- 
    output_04$cv_results %>%
      extract_workflow(
        best_wflow_id
      ) %>% 
      # finalize_workflow(
      #   output_04$cv_results_best
      # ) %>% 
      fit(df_train_split_comb)
  
  last_fit_preds <- 
    last_fit %>% 
    predict(new_data = df_test_split) %>% 
    mutate(
      .pred = expm1(.pred)
    ) %>% 
    bind_cols(df_test_split)
  
} else {
  
  performance_est <-
    output_04$cv_results %>%
    extract_workflow(
      best_wflow_id
    ) %>%
    # finalize_workflow(
    #   output_04$cv_results_best
    # ) %>%
    last_fit(
      split = output_01$data_split,
      metrics = output_04$my_metric_set
    )
    
}

# Performance
best_wflow_estimate <- 
  if (comb) {
    rmsle(
      truth = cost,
      estimate = .pred,
      data = last_fit_preds
    )
  } else {
    performance_est %>% 
      collect_metrics()
  }

# Predictions
df_test_preds <- 
  if (comb) {
      
    last_fit_preds
    
  } else {
    
    performance_est %>% 
    collect_predictions()
    
  }

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
  best_wflow_estimate %>% 
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
  fit(df_train_comb)

subm_pred <- 
  wflow_final_fit %>% 
  predict(
    new_data = output_01$df_test
  ) %>% 
  mutate(
    .pred = expm1(.pred)
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

# -- save tracking data
save(
  df_expTracking,
  file = "./Output/04a_tracking.RData"
)
