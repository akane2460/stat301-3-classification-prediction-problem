## rf tuning for ensemble-- simple

# load packages ----
library(tidymodels)
library(tidyverse)
library(here)
library(tictoc)
library(stacks)
library(doMC)

# load data ----
load(here("data/train_folds.rda"))
load(here("recipes/simple_recipe.rda"))

# handle common conflicts ----
tidymodels_prefer()

set.seed(5649)

# parallel processing ----
num_cores <- parallel::detectCores(logical = TRUE)
registerDoMC(cores = num_cores)

# model spec----
rf_spec <- 
  rand_forest(trees = 1500, min_n = tune(), mtry = tune()) |> 
  set_engine("ranger") |> 
  set_mode("classification")

# define workflows ----
rf_wflow <-
  workflow() |> 
  add_model(rf_spec) |> 
  add_recipe(simple_recipe)

# hyperparameter tuning values ----
# check ranges for hyperparameters
hardhat::extract_parameter_set_dials(rf_wflow)

set.seed(5649)
# change hyperparameter ranges
rf_params <- parameters(rf_wflow) |> 
  update(mtry = mtry(c(5, 9)),
         min_n = min_n(c(0, 3))) 

# build tuning grid
rf_grid <- grid_regular(rf_params, levels = 5)

# model tuning ----
set.seed(5649)
rf_tuned_simple <- tune_grid(
  rf_wflow,
  train_folds,
  grid = rf_grid,
  control = control_stack_grid(),
  metrics = metric_set(roc_auc))

# save results ----
save(rf_tuned_simple, file = here("final_attempt_10/results/rf_tuned_simple.rda"))