# ====================================================================================================== #
# Description
#
#  I'm having a hard time obtaining same results as other Kagglers even though I am frankly doing more ..
#  So wtf is wrong? In the script we replicate results from https://www.kaggle.com/code/paddykb/ps-s3e11-pump-up-the-gam
#  and compare with a similar lighgbm
#
# Change log:
#   Ver   Date        Comment
#   1.0   --/--/--    Initial version
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

load("./Output/01_output.RData")
load("./Output/03_output.RData")
load("./Output/04a_tracking.RData")
load("./Output/04_output.RData")

# ------------------------------------------------------------------------------------------------------ #
# PROGRAM
# ------------------------------------------------------------------------------------------------------ #

df_train_comb <- 
  output_01$df_train %>% 
  rows_append(
    y = output_01$df_train_org
  )

df_train_rec <- # bad name ..
  output_03$recipe_gam %>% 
  prep() %>% 
  bake(df_train_comb) %>% 
  as.data.table

df_test_rec <- 
  output_03$recipe_gam %>% 
  prep() %>% 
  bake(output_01$df_test) %>% 
  as.data.table


# ---- GAM experiment ---------------------------------------------------------------------

model_formula <- (
  log1p(cost) ~ 
    # interaction between the store and person demographic
    + s(store_sqft, total_children, num_children_at_home, avg_cars_at.home, bs='re')
  # store attributes
  #   Why should these have any effect?
  #   possibly related to which stores are involved in cross store campaigns
  + coffee_bar
  + video_store
  + prepared_food * florist)

# -- Models
bam_test <-
  mgcv::bam(
    model_formula,
    data = df_train_rec,
    descrete = TRUE
  )

# -- predict
bam_pred <- 
  mgcv::predict.bam( # predict can end up using the wrong method ...
    bam_test,
    newdata = df_test_rec,
    type = "response"
  ) %>% 
  expm1

# -- submission
df_subm <- 
  df_test_rec %>%
  mutate(
    cost = bam_pred
  ) %>% 
  select(
    id,
    cost
  ) 

df_subm %>% 
  fwrite(file = "./Output/05_submission_bam.csv") 


# ---- Lightgbm comparison -----------------------------------------------------

lgbm_rec <- 
  recipe(
    formula = cost ~ .,
    data = df_train_comb
  ) %>%
  update_role(
    id,
    new_role = "id"
  ) %>% 
  step_rm(
    store_sales,
    unit_sales,
    gross_weight,
    recyclable_package,
    low_fat,
    units_per_case,
    original,
    salad_bar
  ) %>% 
  step_dummy(
    all_ordered_predictors()
  ) %>% 
  step_intercept() %>% 
  step_mutate(
    cost = log1p(cost),
    skip = TRUE
  ) %>% 
  step_interact(
    terms = ~ prepared_food:florist + store_sqft:starts_with("total_children"):starts_with("avg_cars")
  ) %>%  
  step_ns(
    store_sqft,
    deg_free = 4
  ) 
  
lgbm_rec_test <- 
  lgbm_rec %>% 
  prep() %>% 
  bake(df_train_comb) %>% 
  as.data.table

lgbm_spec <- 
  boost_tree(
    tree_depth = 8, 
    learn_rate = 0.1, 
    loss_reduction = 0.1, 
    min_n = 15, 
    sample_size = 0.6, 
    trees = 3500
  ) %>% 
  set_mode("regression") %>% 
  set_engine(
    "lightgbm"
  ) 

lgbm_wflow <- 
  workflow() %>% 
  add_recipe(lgbm_rec) %>% 
  add_model(lgbm_spec)

lgbm_fit <- 
  lgbm_wflow %>% 
  fit(data = df_train_comb)

# -- predict
lgbm_pred <- 
  predict( # predict can end up using the wrong method ...
    lgbm_fit,
    new_data = output_01$df_test
  ) %>% 
  expm1

# -- submission
df_subm <- 
  output_01$df_test %>%
  cbind(
    lgbm_pred
  ) %>% 
  select(
    id,
    "cost" = .pred
  ) 

df_subm %>% 
  fwrite(file = "./Output/05_submission_bam_lgbm.csv") 
