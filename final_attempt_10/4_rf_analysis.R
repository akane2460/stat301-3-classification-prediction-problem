# Analysis Simple

## Load Packages ----
library(tidyverse)
library(tidymodels)
library(skimr)
library(janitor)
library(here)
library(knitr)
library(corrplot)
library(stacks)

tidymodels_prefer()

# read in the data----
load(here("data/train_classification_cleaned.rda"))
load(here("data/test_classification_cleaned.rda"))

################################################################################
################################################################################
################################################################################
# simple rf fit----
load(here("final_attempt_10/results/simple_fit_rf.rda"))

final_attempt_10_submission <-
  bind_cols(test_classification_cleaned,
            predict(simple_fit_rf,
                    test_classification_cleaned,
                    type = "prob")) |>
  select(id, .pred_TRUE) |>
  rename(predicted = .pred_TRUE)

final_attempt_10_submission$predicted <- format(final_attempt_10_submission$predicted, scientific = FALSE)

write_csv(final_attempt_10_submission, file = here("final_attempt_10/results/final_attempt_10_submission.csv"))

# save to final submissions folder
write_csv(final_attempt_10_submission, file = here("final_submissions/final_attempt_10_submission.csv"))

################################################################################
################################################################################
################################################################################
