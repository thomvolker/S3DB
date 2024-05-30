library(haven)
library(readr)

dataset <- read_csv("sport_psychology/Chen_1_2024/preproc/study1_analysis_release.csv")
write_csv(dataset, "sport_psychology/Chen_1_2024/dataset.csv")

