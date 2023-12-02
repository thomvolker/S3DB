
library(haven)

dataset <- read_sav("prochazka2022/preproc/Pain_S1pilot_Czechia.sav")
write.csv(dataset, "prochazka2022/dataset.csv", row.names = FALSE)
