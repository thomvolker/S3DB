library(haven)
library(readr)
library(readxl)

dataset <- read_csv("sport_psychology/Emanuel_1_24/preproc/exp1_fixed_varied_grip.csv")
write_csv(dataset, "sport_psychology/Emanuel_1_24/dataset.csv")


View(dataset)
