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
library(bonsai) # lightgbm

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

# using the combined data yields much better results
df_train_split_comb <- 
  df_train_split %>% 
  rows_append(
    y = output_01$df_train_org
  )

cost_log <- TRUE # do we use rmse as loss? Then we log1p transform

if (cost_log) { # we cannot transform predictions in tune_grid, so this is easier than step_mutate(cost = log1p(cost), skip = TRUE)
  
  df_train_split_comb[
    ,
    cost := log1p(cost)
  ]
  
}

my_metric <- 
  ifelse(
    cost_log,
    "rmse",
    "rmsle"
  )

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
    objective = "reg:squaredlogerror",
    lambda = 0.02, # L2 reg
    alpha = 9e-7, # L1 reg
    colsample_bytree = 0.7,
    counts = FALSE
  ) 

# -- GLM
glm_spec <- 
  linear_reg(
    penalty = 1
  ) %>% 
  set_mode("regression") %>% 
  set_engine(
    "glmnet"
    #family = Gamma(link = "inverse")
  )

# -- Light GBM
lightgbm_spec <- 
  boost_tree(
    tree_depth = 8, 
    learn_rate = 0.1, 
    loss_reduction = 0.1, 
    min_n = 15, 
    sample_size = 0.6, 
    trees = 4000
  ) %>% 
  set_mode("regression") %>% 
  set_engine(
    "lightgbm"
  ) 

# -- GAM
gam_spec <- # takes way too long to tune ...
  gen_additive_mod(
    mode = "regression"
  )

# ---- Workflow set ------------------------------------------------------------

wflow <- 
  workflow_set(
    preproc = 
      list(
        "recipe_play" = output_03$recipe_play
        #"recipe_play" = output_03$recipe_play
        #"recipe_2_comb" = output_03$recipe_2_comb
      ),
    models = 
      list(
        #"gam" = gam_spec # way too slow 
        #"xgboost" = xgb_spec,
        #"glm" = glm_spec,
        "lightgbm" = lightgbm_spec
      )
  )

# GAM update formula
# wflow %<>% 
#   update_workflow_model(
#     id = "recipe_1_gam",
#     spec = gam_spec,
#     formula = 
#       log1p(cost) ~ 
#       s(store_sqft, total_children, num_children_at_home, avg_cars_at.home, bs='re')
#     + coffee_bar
#     + video_store
#     + prepared_food * florist
#   )

# it is necessary to create wflow_comb for models using the combined data, 
# because the data used to create resamples is fixed (we cannot simply pass the data from the recipe, unfortunately)

# ---- Tune models -------------------------------------------------------------

my_metric_set <- 
  metric_set(
    rmse
    #rmsle
  )
  
tic("grid search")
cv_results <- 
  wflow %>% 
  workflow_map(
    # fit_resamples is used automatically when no parameters are tagged with tune()
    #"tune_race_anova", # faster, more crude, tuning compared to grid search
    seed = 30349,
    resamples = 
      vfold_cv(
        df_train_split_comb, 
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

# ---- Compare models ----------------------------------------------------------

# -- Best model
cv_results %>% 
  autoplot(
    metric = my_metric,
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
tune_switch <- FALSE
if (tune_switch) {
  
  cv_results %>% 
    autoplot(
      id = "recipe_play_lightgbm",
      metric = "rmse"
    )
  
}

# -- Analysis of residuals
cv_best_res <- 
  cv_results %>% 
  filter(
    wflow_id == "recipe_play_lightgbm"
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
      select_best(metric = "rmse")
  )

exp_results$metrics <- 
  exp_results$wflow_name %>% 
  purrr::set_names() %>%
  map(
    ~ cv_results %>% 
      workflowsets::rank_results(select_best = TRUE) %>% 
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
