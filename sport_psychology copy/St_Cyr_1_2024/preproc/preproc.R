library(haven)
library(readr)
library(dplyr)


dataset <- read_csv("sport_psychology/St_Cyr_1_2024/preproc/3-Study1-attitudes_behaviors_athletes_path.csv")
View(dataset)

# remove the unnecessary columns from the dataset:
dataset <- dataset |>
  dplyr::select(SOP, SPP, HP, OP, eat_disorder, psy_wb, PHYS_H)

write_csv(dataset, "sport_psychology/St_Cyr_1_2024/dataset.csv")

