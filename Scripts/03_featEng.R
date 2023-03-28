# ====================================================================================================== #
# Description
#
#   Feature engineering: implementing findings from EDA
#
# Change log:
#   Ver   Date        Comment
#   1.0   23/03/23    Initial version
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

# ------------------------------------------------------------------------------------------------------ #
# PROGRAM
# ------------------------------------------------------------------------------------------------------ #

df_train_split <- training(output_01$data_split)

# ---- Feature Engineering -----------------------------------------------------

# -- Recipe base
#    Minimal feature engineering - all following recipes builds on this one

recipe_base <- 
  recipe(
    formula = cost ~ .,
    data = df_train_split
  ) %>% 
  # -- update roles
  update_role(
    id,
    new_role = "id"
  ) %>% 
  # -- remove predictors
  step_rm(
    salad_bar # same as prepared food
  )

# -- Recipe 1
#    Minimal feature engineering 

recipe_1 <- 
  recipe_base %>% 
  step_dummy( # creates polynomial contrasts because they are ordered factors. 
    all_ordered_predictors()
  )  %>% 
  step_intercept()

rec_1_test <- 
  recipe_1 %>% 
  prep() %>% 
  bake(df_train_split) %>% 
  as.data.table

# -- Recipe 2
#    Manual feature generation
#    Collapse levels

recipe_2 <- 
  recipe_base %>% 
  # -- manual feature creation
  step_mutate( 
    children_ratio = 
      pmin(as.integer(num_children_at_home) / as.integer(total_children), 1), # there are 4 faulty records
    sales_per_unit = 
      store_sales / (as.integer(unit_sales)),
    sales_per_sqft = 
      store_sales / store_sqft,
    units_per_sqft = 
      as.integer(unit_sales) / store_sqft,
    weight_per_unit = 
      gross_weight / as.integer(unit_sales)  
  ) %>% 
  # -- collapse factor levels: make sure that the right order of the levels is preserved
  step_mutate(
    num_children_at_home = 
      fct_collapse(
        num_children_at_home,
        gte_3 = c("3", "4", "5")
      ),
    unit_sales = 
      fct_collapse(
        unit_sales,
        gte_4 = c("4", "5", "6"),
        lte_2 = c("1", "2")
      )
  ) %>% 
  step_normalize(
    all_double_predictors() # use double to avoid transforming indicator vars
  ) %>% 
  step_dummy( # creates polynomial contrasts because they are ordered factors. 
    all_ordered_predictors()
  )  %>% 
  step_intercept()

# Test to see that all went as expected (the selector functions used in recipes sometimes capture unintended variables)
rec_2_test <- 
  recipe_2 %>% 
  prep() %>% 
  bake(df_train_split) %>% 
  as.data.table

# -- Recipe 3
#    Interactions. Note that interactions should be created after step_dummy, so we can build directly on recipe_1
#    The terms are selected based on EDA variable importance

recipe_3 <- 
  recipe_base %>% 
  # -- collapse factor levels: make sure that the right order of the levels is preserved
  step_mutate(
    num_children_at_home = 
      fct_collapse(
        num_children_at_home,
        gte_3 = c("3", "4", "5")
      ),
    unit_sales = 
      fct_collapse(
        unit_sales,
        gte_4 = c("4", "5", "6"),
        lte_2 = c("1", "2")
      )
  ) %>% 
  step_normalize(
    all_double_predictors() # use double to avoid transforming indicator vars
  ) %>% 
  step_dummy( # creates polynomial contrasts because they are ordered factors. 
    all_ordered_predictors()
  )  %>% 
  step_interact(
    terms = 
      ~ store_sales:starts_with("avg_") +
        store_sales:starts_with("total_") +
        gross_weight:starts_with("total_") +
        gross_weight:starts_with("avg_") +
        store_sales:starts_with("unit_")
  ) %>% 
  step_intercept()

rec_3_test <- 
  recipe_3 %>% 
  prep() %>% 
  bake(df_train_split) %>% 
  as.data.table


# ==== EXPORT ------------------------------------------------------------------------------------------ 

output_03 <- 
  list(
    "recipe_base" = recipe_base,
    "recipe_1" = recipe_1,
    "recipe_2" = recipe_2,
    "recipe_3" = recipe_3
  )  

save(
  output_03,
  file = "./Output/03_output.RData"
)
