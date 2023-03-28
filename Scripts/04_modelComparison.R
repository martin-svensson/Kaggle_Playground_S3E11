# ====================================================================================================== #
# Description
#
#   Model comparison through resampling techniques (cross validation) and
#   workflow sets (combining feature engineering and models)
#   Run initTracking before this script to initialize the tibble for tracking
#
# Change log:
#   Ver   Date        Comment
#   1.0   25/03/23    Initial version
#
# ====================================================================================================== #
# ------------------------------------------------------------------------------------------------------ #
# LIBRARIES
# ------------------------------------------------------------------------------------------------------ #

library(data.table)
library(tidyverse)
library(magrittr)

library(tidymodels)
library(finetune) # race approach

library(tictoc)

# ------------------------------------------------------------------------------------------------------ #
# IMPORT AND SOURCES
# ------------------------------------------------------------------------------------------------------ #

load("./Output/01_output.RData")
load("./Output/03_output.RData")
load("./Output/04a_tracking.RData")

# -- Custom metric
source("./Scripts/04b_customMetrics.R")

# ------------------------------------------------------------------------------------------------------ #
# PROGRAM
# ------------------------------------------------------------------------------------------------------ #

df_train_split <- training(output_01$data_split)

# ---- Model Specifications ----------------------------------------------------

# -- xgboost
xgb_spec <- 
  boost_tree(
    tree_depth = 12, 
    learn_rate = 0.9, 
    loss_reduction = 0.01, 
    min_n = 30, 
    sample_size = 0.6, 
    trees = 350
  ) %>% 
  set_mode("regression") %>% 
  set_engine(
    "xgboost",
    # loss function in correspondence with evaluation. We may want to treat it as a hyperparameter
    objective = "reg:squaredlogerror",
    lambda = 0.02, # L2 reg
    alpha = 9e-7, # L1 reg
    colsample_bytree = 0.7,
    counts = FALSE
  ) 


# ---- Workflow set ------------------------------------------------------------

wflow <- 
  workflow_set(
    preproc = 
      list(
        "recipe_1" = output_03$recipe_1,
        "recipe_2" = output_03$recipe_2,
        "recipe_3" = output_03$recipe_3
      ), 
    models = 
      list(
        "xgboost" = xgb_spec
      )
  )


# ---- Tune models -------------------------------------------------------------

my_metric_set <- 
  metric_set(
    rmsle
  )

tune_switch <- FALSE
# TRUE: hyperparameter tuning for parameters tagged with tune()
# FALSE: CV error estimate. Useful for comparing recipes without tuning any parameters

if (tune_switch) {
  
  tic("grid search")
  cv_results <- 
    wflow %>% 
    workflow_map(
      #"tune_race_anova", # faster, more crude, tuning compared to grid search
      seed = 30349,
      resamples = 
        vfold_cv(
          df_train_split, 
          v = 5,
          strata = cost
        ),
      grid = 5,
      control = 
        control_grid( # use control_race to use the race methodology
          save_pred = TRUE,
          parallel_over = NULL, # automatically chooses betweeen "resamples" and "everything"
          save_workflow = FALSE
        ),
      metrics = my_metric_set
    )
  toc()
  
} else {
  
  tic("CV error estimate")
  cv_results <- 
    wflow %>% 
    workflow_map(
      "fit_resamples",
      seed = 92764,
      resamples = 
        vfold_cv(
          df_train_split, 
          v = 5,
          strata = cost
        ),
      control = 
        control_resamples(
          save_pred = TRUE,
          parallel_over = NULL, # automatically chooses betweeen "resamples" and "everything"
          save_workflow = FALSE
        ),
      metrics = my_metric_set
    )
  toc()
  
}



# ---- Compare models ----------------------------------------------------------

# -- Best model
cv_results %>% 
  autoplot(
    metric = "rmsle",
    select_best = TRUE
  ) + 
  geom_text(
    aes(
      y = mean,
      label = wflow_id
    ), 
    angle = 90, 
    nudge_x = 0.05,
    color = "black",
    size = 3
  ) +
  theme(legend.position = "none")

# -- Parameter values for xgboost
if (tune_switch) {
  
cv_results %>% 
  autoplot(
    id = "recipe_2_xgboost",
    metric = "rmsle"
  )
  
}


# -- Analysis of residuals
cv_best_res <- 
  cv_results %>% 
  filter(
    wflow_id == "recipe_1_xgboost"
  ) %>% 
  .$result %>% 
  .[[1]] %>% # there is probably a better way to extract the results
  collect_predictions() %>% 
  mutate(
    res = (log(1 + cost) - log(1 + .pred))^2
  )

cv_best_res %>% 
  ggplot(aes(x = cost, y = .pred)) +
  stat_bin_hex() +
  geom_abline(color = "blue")

cv_best_res %>% 
  ggplot(aes(x = res)) + 
  geom_histogram()

# Inspect observations which were severely under- or overpredicted
df_large_res <- 
  cv_best_res %>% 
  filter(
    res > quantile(res, probs = 0.99)
  ) %>% 
  arrange(
    desc(res)
  ) 

df_res_inspect <- 
  df_train_split %>%
  dplyr::slice(df_large_res$.row) %>% 
  unique %>% 
  select(!c(id, cost))  

df_res_inspect %>%
  plot_histogram()

df_res_inspect %>%
  plot_bar()
# Compare to the EDA


# ---- Track experiment --------------------------------------------------------

exp_results <- 
  tibble(
    "time" = 
      rep_along(
        cv_results$wflow_id, 
        lubridate::now()
      ),
    "wflow_name" = cv_results$wflow_id,
    "val.or.test" = 
      rep_along(
        cv_results$wflow_id, 
        "CV results"
      )
  )

exp_results$recipe <- 
  exp_results$wflow_name %>% 
  purrr::set_names() %>% 
  map(
    ~ extract_preprocessor(
        x = cv_results,
        id = .x
      ) %>% 
      butcher::butcher() # reduce memory demand
  )

exp_results$model.spec <- 
  exp_results$wflow_name %>% 
  purrr::set_names() %>% 
  map(
    ~ extract_spec_parsnip(
        x = cv_results,
        id = .x
      )
  )

exp_results$tuned.par <- 
  exp_results$wflow_name %>% 
  purrr::set_names() %>% 
  map(
    ~ cv_results %>% 
      extract_workflow_set_result(.x) %>% 
      select_best(metric = "rmsle")
  )

exp_results$metrics <- 
  exp_results$wflow_name %>% 
  purrr::set_names() %>%
  map(
    ~ cv_results %>% 
      rank_results(select_best = TRUE) %>% 
      select(
        wflow_id,
        .metric,
        mean,
        std_err
      ) %>% 
      filter(
        wflow_id == .x
      )
  )

# -- Append to tracking data
df_expTracking %<>% 
  dplyr::rows_append(
    y = exp_results
  )



# ==== EXPORT ------------------------------------------------------------------------------------------ 

output_04 <- 
  list(
    "my_metric_set" = my_metric_set,
    "cv_results" = cv_results
  )

save(
  output_04,
  file = "./Output/04_output.RData"
)

# -- save tracking data
save(
  df_expTracking,
  file = "./Output/04a_tracking.RData"
)
