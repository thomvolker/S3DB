library(haven)
library(readr)
library(tidyverse)

load("sport_psychology/Ziv_2024/preproc/expectation_data.RData")

# rename "data" as dataset
dataset <- data

# remove "data"
rm(data)


dataset <- dataset %>%
  pivot_longer(cols = c(stretch_balance_belief, relaxation_belief, walking_belief,
                        resistance_belief, lectures_belief, yoga_taichi_belief),
               names_to = "belief_type", values_to = "score")

write_csv(dataset, "sport_psychology/Ziv_2024/dataset.csv")

view(dataset)
