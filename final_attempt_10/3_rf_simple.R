# Define and fit rf

# random processes present

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(doMC)
library(parallel)

# handle common conflicts
tidymodels_prefer()

# parallel processing
num_cores <- parallel::detectCores(logical = TRUE)

registerDoMC(cores = num_cores)

# read in the data----
load(here("data/train_classification_cleaned.rda"))

# load the recipes----
load(here("recipes/simple_recipe.rda"))

# load tuned result----
load(here("final_attempt_10/results/rf_tuned_simple.rda"))

# set seed
set.seed(0129)

# tuned analysis----
# best results
best_results_rf <- select_best(rf_tuned_simple, metric = "roc_auc")

best_results_rf |> knitr::kable()

# best parameters: mtry = 9, min_n = 1

# model specifications----
rf_spec <- 
  rand_forest(trees = 1500, min_n = 1, mtry = 9) |> 
  set_engine("ranger") |> 
  set_mode("classification")

# define workflows ----
rf_wflow <-
  workflow() |> 
  add_model(rf_spec) |> 
  add_recipe(simple_recipe)

# fit workflows/models ----
simple_fit_rf <- fit(rf_wflow, train_classification_cleaned)

# save tune
save(simple_fit_rf, file = here("final_attempt_10/results/simple_fit_rf.rda"))