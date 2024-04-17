
library(haven)
library(readr)

dataset <- read_sav("psychology/prochazka2022_2/preproc/Pain_S2registered_Slovakia.sav")
write_csv(dataset, "psychology/prochazka2022_2/dataset.csv")
