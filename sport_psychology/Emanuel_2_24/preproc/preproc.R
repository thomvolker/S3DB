library(haven)
library(readr)
library(readxl)

dataset <- read_csv("sport_psychology/Emanuel_2_24/preproc/exp2_fixed_varied_grip.csv")
write_csv(dataset, "sport_psychology/Emanuel_2_24/dataset.csv")

View(dataset)
