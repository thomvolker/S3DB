library(haven)
library(readr)


dataset <- read_csv("sport_psychology/Martin_2024/preproc/combined-thk-eng-prod.csv")
write_csv(dataset, "sport_psychology/Martin_2024/dataset.csv")
View(dataset)
