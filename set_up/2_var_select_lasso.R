# Variable Selection ----
# Variable selection using lasso

# load package(s) ----
library(tidyverse)
library(tidymodels)
library(here)
library(doMC)

# handle common conflicts
tidymodels_prefer()

# parallel processing ----
num_cores <- parallel::detectCores(logical = TRUE)
registerDoMC(cores = num_cores - 1)

# create resamples/folds ----
load(here("data/train_classification_cleaned.rda"))

set.seed(0124)
lasso_folds <- 
  train_classification_cleaned |> 
  vfold_cv(v = 5, repeats = 3)


# basic recipe ----
simple_recipe <- recipe(
  host_is_superhost ~ ., data = train_classification_cleaned) |> 
  step_rm(id) |> 
  step_impute_median(all_numeric_predictors()) |>
  step_impute_mode(all_nominal_predictors()) |>
  step_dummy(all_nominal_predictors()) |> 
  step_novel(all_nominal_predictors()) |> 
  step_corr(all_numeric_predictors()) |>  
  step_nzv(all_predictors())  |> 
  step_normalize(all_predictors())

# checking the recipe 
# simple_recipe |> 
#   prep() |> 
#   bake(new_data = NULL)

# model specifications ----
lasso_spec <-
  logistic_reg(
    mixture = 1,
    penalty = tune()
  ) |> 
  set_mode("classification") |> 
  set_engine("glmnet")

# define workflows ----
lasso_wflow <-
  workflow() |> 
  add_model(lasso_spec) |> 
  add_recipe(simple_recipe)

# hyperparameter tuning values ----
hardhat::extract_parameter_set_dials(lasso_spec)

lasso_params <- hardhat::extract_parameter_set_dials(lasso_spec) |> 
  update(
    penalty = penalty(c(-3, 0))
  )

# build tuning grid
lasso_grid <- grid_regular(lasso_params, levels = 5)

# fit workflow/model ----
lasso_tuned <- 
  lasso_wflow |> 
  tune_grid(
    resamples = lasso_folds, 
    grid = lasso_grid,
    metrics = metric_set(roc_auc),
    control = control_grid(save_workflow = TRUE)
  )

# extract best model (optimal tuning parameters)
optimal_wflow <- 
  extract_workflow(lasso_tuned) |> 
  finalize_workflow(select_best(lasso_tuned, metric = "roc_auc"))

# fit best model/results
var_select_fit_lasso <- fit(optimal_wflow, train_classification_cleaned)

# look at results
var_select_lasso <- var_select_fit_lasso |>  tidy()

var_select_lasso |> filter(estimate != 0) |> select(term) |> knitr::kable()


# write out variable selection results ----
save(
  var_select_fit_lasso, 
  file = here("results/var_select_fit_lasso.rda")
)
# 
load(here("results/var_select_fit_lasso.rda"))

var_select_lasso <- var_select_fit_lasso |>  tidy()

var_select_lasso |> filter(abs(estimate) < .01) |> select(term) |> knitr::kable()
  # ones to remove potentially: longitude, accommodates, maximum_nights, minimum_maximum_nights, 
  # availability, number_of_reviews, review_scores_checkin, property_type, bathrooms_text
