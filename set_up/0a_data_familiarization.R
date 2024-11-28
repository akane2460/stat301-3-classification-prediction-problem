# Data Familiarization

## Load Packages ----
library(tidyverse)
library(tidymodels)
library(skimr)
library(janitor)
library(here)
library(knitr)
library(corrplot)
library(caret)

tidymodels_prefer()

# read in the data----
load(here("data/train_classification_cleaned.rda"))

# general data skim
skimr::skim_without_charts(train_classification_cleaned)

# missing data in host_location, host_response_time, host_response_rate, 
# host_neighborhood, host_acceptance_rate, bathrooms_text, first_review
# last_review, beds, review_scores_rating, review_scores_accuracy, 
# review_scores_cleanliness, review_scores_checkin, review_scores_communication
# review_scores_location, review_scores_value, reviews_per_month

# target variable exploration----
superhost_dist_plot <- train_classification_cleaned |> 
  ggplot(aes(x = host_is_superhost, fill = host_is_superhost)) +
  geom_bar() +
  labs(
    title = "Airbnb Superhosts",
    x = "Superhost Identity",
    y = "Count"
  ) +
  theme_minimal()

ggsave(filename = here("plots/superhost_dist_plot.png"), superhost_dist_plot)

# exploring potential interactions----
train_numeric <- train_classification_cleaned |> 
  select(where(is.numeric)) |> 
  select(-longitude, -host_response_time, -host_response_rate, -beds, -host_acceptance_rate, 
         -first_review, -last_review, -review_scores_rating, 
         -review_scores_accuracy, -review_scores_cleanliness,
         -review_scores_checkin, -review_scores_communication, -review_scores_location,
         -review_scores_value, -reviews_per_month) |> 
  select(-maximum_nights, -minimum_maximum_nights, -availability_90,
         -number_of_reviews_l30d)

correlation_matrix <- cor(train_numeric) 

cor_threshold <- 0.5
# Find highly correlated variable pairs
highly_correlated <- findCorrelation(correlation_matrix, cutoff = cor_threshold, verbose = TRUE)

train_numeric_high_corr <- train_classification_cleaned |> 
  select(maximum_maximum_nights,
         minimum_nights_avg_ntm,
         minimum_minimum_nights,
         availability_30,
         maximum_minimum_nights,
         calculated_host_listings_count_entire_homes,
         calculated_host_listings_count,
         host_listings_count,
         availability_365,
         number_of_reviews_ltm,)

correlation_matrix_high <- cor(train_numeric_high_corr) 

# potential interactions
# host_listings_count x calculated_host_listings_count
# host_listings_count x calculated_host_listings_count_entire_homes
# host_listings_count x maximum_minimum_nights
# host_listings_count x minimum_nights_avg_ntm
# host_listings_count x host_total_listings_count
# host_total_listings_count x calculated_host_listings_count  
# host_total_listings_count x calculated_host_listings_count_entire_homes
#  minimum_minimum_nights x minimum_nights
# minimum_nights x minimum_nights_avg_ntm 
#  minimum_minimum_nights x minimum_nights_avg_ntm 
# maximum_maximum_nights x host_total_listings_count 
# maximum_maximum_nights x minimum_nights_avg_ntm 
# maximum_minimum_nights x minimum_nights_avg_ntm 
# maximum_maximum_nights x maximum_nights_avg_ntm 
# maximum_minimum_nights x calculated_host_listings_count 
# maximum_minimum_nights x calculated_host_listings_count_entire_homes 
# minimum_nights_avg_ntm x host_listings_count
# minimum_nights_avg_ntm x host_total_listings_count
# minimum_nights_avg_ntm x maximum_minimum_nights
# minimum_nights_avg_ntm x calculated_host_listings_count
# minimum_nights_avg_ntm x calculated_host_listings_count_entire_homes
# availability_30 x availability_60
# number_of_reviews_ltm x number_of_reviews
# calculated_host_listings_count x host_listings_count
# calculated_host_listings_count x host_total_listings_count
# calculated_host_listings_count x maximum_minimum_nights 
# calculated_host_listings_count x minimum_nights_avg_ntm 
# calculated_host_listings_count x calculated_host_listings_count_entire_homes
# calculated_host_listings_count_entire_homes x host_listings_count
# calculated_host_listings_count_entire_homes x host_total_listings_count
# calculated_host_listings_count_entire_homes x maximum_minimum_nights
# calculated_host_listings_count_entire_homes x minimum_nights_avg_ntm
# calculated_host_listings_count_entire_homes x  calculated_host_listings_count

# correlation_matrix_high |>
#   knitr::kable()

corrplot(correlation_matrix_high)

train_classification_cleaned |> skimr::skim_without_charts()

train_classification_cleaned |> 
  ggplot(aes(x = room_type)) +
  geom_bar() +
  facet_wrap(~ property_type)
  # a bit of an interaction

train_classification_cleaned |> 
  ggplot(aes(x = bathrooms_text)) +
  geom_bar() +
  facet_wrap(~ property_type)
  # not that much of an interaction

train_classification_cleaned |> 
  ggplot(aes(x = host_verifications)) +
  geom_bar() +
  facet_wrap(~ host_has_profile_pic)
  # interaction possibly

train_classification_cleaned |> 
  ggplot(aes(x = host_neighbourhood)) +
  geom_bar() +
  facet_wrap(~ property_type)
    # likely interaction

train_classification_cleaned |> 
  ggplot(aes(x = host_neighbourhood)) +
  geom_bar() +
  facet_wrap(~ property_type)
  # likely interaction
  
train_classification_cleaned |> 
  ggplot(aes(x = reviews_per_month, fill = property_type)) +
  geom_boxplot() 
  # some variation here, potential interaction

train_classification_cleaned |> 
  ggplot(aes(x = reviews_per_month, fill = room_type)) +
  geom_boxplot() 
# some variation here, potential interaction

train_classification_cleaned |> 
  ggplot(aes(x = review_scores_rating, fill = room_type)) +
  geom_boxplot() 
  # some variation here (entire homes and private rooms higher rated typically)

train_classification_cleaned |> 
  ggplot(aes(x = review_scores_location, fill = neighbourhood_cleansed)) +
  geom_boxplot() 
  # some neighbourhoods are "better" than others, get better ratings
