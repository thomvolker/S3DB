
library(haven)
library(readr)

dataset <- read_sav("psychology/prochazka2022_1/preproc/Pain_S1pilot_Czechia.sav")
write_csv(dataset, "psychology/prochazka2022_1/dataset.csv")


