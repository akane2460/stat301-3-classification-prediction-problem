# Recipes

## Load Packages ----
library(tidyverse)
library(tidymodels)
library(skimr)
library(janitor)
library(here)
library(knitr)
library(corrplot)

tidymodels_prefer()

# read in the data----
load(here("data/train_classification_cleaned.rda"))

# recipes----
# simple
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

# prep(simple_recipe) |>
#   bake(new_data = NULL)

save(simple_recipe, file = here("recipes/simple_recipe.rda"))

# advanced
advanced_recipe <- recipe(
  host_is_superhost ~ ., data = train_classification_cleaned) |>
  step_rm(id) |>
  step_impute_median(all_numeric_predictors()) |>
  step_impute_mode(all_nominal_predictors()) |>
  step_dummy(all_nominal_predictors()) |>
  step_novel(all_nominal_predictors()) |>
  step_rm(longitude, accommodates, maximum_nights, minimum_maximum_nights, availability_90,
          number_of_reviews_l30d, host_verifications_X..phone.., neighbourhood_cleansed_Logan.Square,
          neighbourhood_cleansed_Near.North.Side, neighbourhood_cleansed_Near.West.Side,
          neighbourhood_cleansed_West.Town, property_type_Entire.condo,
          property_type_Entire.rental.unit, property_type_Private.room.in.home,
          bathrooms_text_X1.shared.bath) |>
  step_corr(all_numeric_predictors()) |>
  step_nzv(all_predictors())  |>
  step_normalize(all_predictors())


# prep(lasso_advanced_recipe) |>
#   bake(new_data = NULL)

save(advanced_recipe, file = here("recipes/advanced_recipe.rda"))

# additional_advanced_recipe
additional_advanced_recipe <- recipe(
  host_is_superhost ~ ., data = train_classification_cleaned) |>
  step_rm(id) |>
  step_impute_median(all_numeric_predictors()) |>
  step_impute_mode(all_nominal_predictors()) |>
  step_interact(~ minimum_nights_avg_ntm:maximum_minimum_nights) |>
  step_interact(~ minimum_nights_avg_ntm:calculated_host_listings_count_entire_homes) |>
  step_interact(~ minimum_nights_avg_ntm:calculated_host_listings_count) |>
  step_interact(~ minimum_nights_avg_ntm:host_listings_count) |>
  step_interact(~ maximum_minimum_nights:host_listings_count) |>
  # step_interact(~ maximum_minimum_nights:minimum_minimum_nights) |>
  # step_interact(~ maximum_minimum_nights:calculated_host_listings_count) |>
  # step_interact(~ minimum_minimum_nights:minimum_nights_avg_ntm) |>
  step_interact(~ calculated_host_listings_count_entire_homes:calculated_host_listings_count) |>
  step_interact(~ calculated_host_listings_count_entire_homes:host_listings_count) |>
  step_interact(~ availability_30:availability_365) |>
  step_interact(~ latitude:host_location) |> # latitude def related to location
  step_interact(~ latitude:host_neighbourhood) |> # latitude def related to which neighborhood
  step_interact(~ beds:bathrooms_text) |>  # more beds typically more bathrooms
  step_interact(~ beds:room_type) |>
  step_interact(~ property_type:reviews_per_month) |>
  step_interact(~ room_type:reviews_per_month) |>
  step_interact(~ room_type:review_scores_rating) |>
  step_interact(~ review_scores_location:neighbourhood_cleansed) |>
  # step_interact(~ review_scores_location:location) |> # check this !
  step_dummy(all_nominal_predictors()) |>
  step_novel(all_nominal_predictors()) |>
  step_rm(longitude, accommodates, maximum_nights, minimum_maximum_nights, availability_90,
          number_of_reviews_l30d, host_verifications_X..phone.., neighbourhood_cleansed_Logan.Square,
          neighbourhood_cleansed_Near.North.Side, neighbourhood_cleansed_Near.West.Side,
          neighbourhood_cleansed_West.Town, property_type_Entire.condo,
          property_type_Entire.rental.unit, property_type_Private.room.in.home,
          bathrooms_text_X1.shared.bath) |>
  step_corr(all_numeric_predictors()) |>
  step_nzv(all_predictors())  |>
  step_normalize(all_predictors())

# prep(additional_advanced_recipe) |>
#   bake(new_data = NULL)

save(additional_advanced_recipe, file = here("recipes/additional_advanced_recipe.rda"))
