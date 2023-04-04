# ====================================================================================================== #
# Description
#
#   Preprocessing and data split
#
# Change log:
#   Ver   Date        Comment
#   1.0   21/03/23    Initial version
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

df_train <- fread("./Data/train.csv")
df_test <- fread("./Data/test.csv")

df_train_org <- fread("./Data/original/train_dataset.csv")

# ------------------------------------------------------------------------------------------------------ #
# PROGRAM
# ------------------------------------------------------------------------------------------------------ #

df_train %>% str  
df_train %>% summary

df_train_org %>% str
df_train_org %>% summary

# -- Create ID for original data
#    starts at 1e6 to be able to distinguish from df_train

df_train_org %<>% 
  mutate(
    id = 1e6 + (row_number() - 1)
  ) %>% 
  select(
    id,
    everything()
  )

# -- Create indicator for original data

df_train %<>% 
  mutate(
    original = 0L
  )

df_train_org %<>% 
  mutate(
    original = 1L
  )

df_test %<>% 
  mutate(
    original = 0L
  )


# -- Rename 
#    Replace all \\s with \\.

df_train %<>% 
  rename_with(.fn = ~ gsub("\\s", "\\.", .x)) %>% 
  rename_with(.fn = ~ gsub("\\.\\d", "", .x)) %>%  # the number in avg_vars does not work well in formulas
  rename_with(.fn = ~ gsub("\\(.*\\)", "", .x)) # remove () since they do not work well with formulas, nor do they provide value anyway
  
df_test %<>% 
  rename_with(.fn = ~ gsub("\\s", "\\.", .x)) %>% 
  rename_with(.fn = ~ gsub("\\.\\d", "", .x)) %>% 
  rename_with(.fn = ~ gsub("\\(.*\\)", "", .x))


df_train_org %<>% 
  rename_with(.fn = ~ gsub("\\s", "\\.", .x)) %>% 
  rename_with(.fn = ~ gsub("\\.\\d", "", .x)) %>% 
  rename_with(.fn = ~ gsub("\\(.*\\)", "", .x))

# -- Factor encoding
#    A number of predictors are ordinal factors, which we will encode as a 
#    pre processing step to avoid cluttering recipes
#    Note: store_sqft is kept as a continuous predictor for now, as it is unclear how best to use the variable (cat or cont)

vars_factor <- 
  c("avg_cars_at.home",
    "num_children_at_home",
    "total_children",
    "unit_sales")

df_train[
  ,
  (vars_factor) := map(.SD, ~ as_factor(.x) %>% as.ordered),
  .SDcols = vars_factor
]

df_test[
  ,
  (vars_factor) := map(.SD, ~ as_factor(.x) %>% as.ordered),
  .SDcols = vars_factor
]

df_train_org[
  ,
  (vars_factor) := map(.SD, ~ as_factor(.x) %>% as.ordered),
  .SDcols = vars_factor
]

# -- Integer encoding
#    Variables, which are already dummy variables, are integer encoded

vars_binary <- 
  c("recyclable_package", 
    "low_fat",
    "coffee_bar",
    "video_store",
    "salad_bar",
    "prepared_food",
    "florist")

df_train[
  ,
  (vars_binary) := map(.SD, as.integer),
  .SDcols = vars_binary
]

df_test[
  ,
  (vars_binary) := map(.SD, as.integer),
  .SDcols = vars_binary
]

df_train_org[
  ,
  (vars_binary) := map(.SD, as.integer),
  .SDcols = vars_binary
]

# -- Data split
#    We do not inlcude original data in the test  data

set.seed(1682468)

data_split <- 
  df_train %>% 
  initial_split(
    prop = 0.8,
    strata = cost
  )

# ==== EXPORT ------------------------------------------------------------------------------------------ 

output_01 <- 
  list(
    "df_train" = df_train,
    "df_test" = df_test,
    "df_train_org" = df_train_org,
    "data_split" = data_split
  )

save(
  output_01,
  file = "./Output/01_output.RData"
)

