library(haven)
library(readr)

dataset <- read_csv("sport_psychology/St_Cyr_1_2024/preproc/3-Study1-attitudes_behaviors_athletes_path.csv")

write_csv(dataset, "sport_psychology/St_Cyr_1_2024/dataset.csv")

View(dataset)
