# Data Familiarization

# random processes present

## Load Packages ----
library(tidyverse)
library(tidymodels)
library(skimr)
library(janitor)
library(here)
library(knitr)
library(corrplot)
library(lubridate)

tidymodels_prefer()

# read in the data----
train_classification <- read_csv(here("data/train_classification.csv"),
                                 col_types = cols(id = col_character()))
test_classification <- read_csv(here("data/test_classification.csv"),
                                col_types = cols(id = col_character()))

# cleaning----
skimr::skim_without_charts(train_classification) |> 
  filter(n_missing != 0)

skimr::skim_without_charts(train_classification) |> 
  filter(n_missing == 0)

# training
train_classification$host_is_superhost <- 
  as.factor(train_classification$host_is_superhost)

train_classification$host_is_superhost

# host_verifications
train_classification$host_verifications <- 
  as.factor(train_classification$host_verifications)

# neighbourhood_cleansed
train_classification$neighbourhood_cleansed <- 
  as.factor(train_classification$neighbourhood_cleansed)

# property_type
train_classification$property_type <- 
  as.factor(train_classification$property_type)

# room_type
train_classification$room_type <- 
  as.factor(train_classification$room_type)

# host location
train_classification$host_location <- 
  as.factor(train_classification$host_location)
# host_neighbourhood
train_classification$host_neighbourhood <- 
  as.factor(train_classification$host_neighbourhood)

# bathrooms_text
train_classification$bathrooms_text <- 
  as.factor(train_classification$bathrooms_text)

# host_has_profile_pic
train_classification$host_has_profile_pic <- 
  as.factor(train_classification$host_has_profile_pic)

# host_identity_verified
train_classification$host_identity_verified <- 
  as.numeric(train_classification$host_identity_verified)

# has_availability 
train_classification$has_availability <- 
  as.numeric(train_classification$has_availability)

# instant_bookable   
train_classification$instant_bookable <- 
  as.numeric(train_classification$instant_bookable)

# host_has_profile_pic
train_classification$host_has_profile_pic <- 
  as.factor(train_classification$host_has_profile_pic)

# host_response_time
train_classification$host_response_time <- 
  gsub("[^0-9.]", "", train_classification$host_response_time)

train_classification$host_response_time <- 
  as.numeric(train_classification$host_response_time)

# host_response_rate 
train_classification$host_response_rate <- 
  gsub("[^0-9.]", "", train_classification$host_response_rate)

train_classification$host_response_rate <- 
  as.numeric(train_classification$host_response_rate)

# host_acceptance_rate 
train_classification$host_acceptance_rate <- 
  gsub("[^0-9.]", "", train_classification$host_acceptance_rate)

train_classification$host_acceptance_rate <- 
  as.numeric(train_classification$host_acceptance_rate)

# host_since year
train_classification$host_since <- year(train_classification$host_since)

# first_review year
train_classification$first_review <- year(train_classification$first_review)

# last_review year
train_classification$last_review <- year(train_classification$last_review)

train_classification_cleaned <- train_classification

save(train_classification_cleaned, file = here("data/train_classification_cleaned.rda"))

# testing----
# host_verifications
test_classification$host_verifications <- 
  as.factor(test_classification$host_verifications)

# neighbourhood_cleansed
test_classification$neighbourhood_cleansed <- 
  as.factor(test_classification$neighbourhood_cleansed)

# property_type
test_classification$property_type <- 
  as.factor(test_classification$property_type)

# room_type
test_classification$room_type <- 
  as.factor(test_classification$room_type)

# host location
test_classification$host_location <- 
  as.factor(test_classification$host_location)
# host_neighbourhood
test_classification$host_neighbourhood <- 
  as.factor(test_classification$host_neighbourhood)

# bathrooms_text
test_classification$bathrooms_text <- 
  as.factor(test_classification$bathrooms_text)

# host_has_profile_pic
test_classification$host_has_profile_pic <- 
  as.factor(test_classification$host_has_profile_pic)

# host_identity_verified
test_classification$host_identity_verified <- 
  as.numeric(test_classification$host_identity_verified)

# has_availability 
test_classification$has_availability <- 
  as.numeric(test_classification$has_availability)

# instant_bookable   
test_classification$instant_bookable <- 
  as.numeric(test_classification$instant_bookable)

# host_response_time
test_classification$host_response_time <- 
  gsub("[^0-9.]", "", test_classification$host_response_time)

test_classification$host_response_time <- 
  as.numeric(test_classification$host_response_time)

# host_response_rate 
test_classification$host_response_rate <- 
  gsub("[^0-9.]", "", test_classification$host_response_rate)

test_classification$host_response_rate <- 
  as.numeric(test_classification$host_response_rate)

# host_acceptance_rate 
test_classification$host_acceptance_rate <- 
  gsub("[^0-9.]", "", test_classification$host_acceptance_rate)

test_classification$host_acceptance_rate <- 
  as.numeric(test_classification$host_acceptance_rate)

# host_since year
test_classification$host_since <- year(test_classification$host_since)

# first_review year
test_classification$first_review <- year(test_classification$first_review)

# last_review year
test_classification$last_review <- year(test_classification$last_review)

test_classification_cleaned <- test_classification

save(test_classification_cleaned, file = here("data/test_classification_cleaned.rda"))

# folds data
# set.seed(0972423)
# train_folds <- train_classification_cleaned |>
#   vfold_cv(v = 5, repeats = 3)
# 
# save(train_folds, file = here("data/train_folds.rda"))
