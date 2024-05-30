library(readxl)
dataset <- read_excel("sport_psychology/Cheval_2_2024/preproc/Physical Effort Scale Data Fall 2022.xlsx")

### Following preprocessing is made according from the code shared in OSF


# rename variables

pes <- dataset
pes <- rename.variable(pes, "Consent 1","consent_1")
pes <- rename.variable(pes, "Consent 2","consent_2")
pes <- rename.variable(pes, "Q60","PA_profile")

## Apply inclusion and exclusion criteria
pes <- pes[2:919,]
pes <- subset(pes, Finished == "True")
pes <- subset(pes, consent_1 == "Yes" & consent_2 == "Yes")
pes <- subset(pes, UserLanguage == "EN")
pes <- subset(pes, Finished == "True")
pes <- subset(pes, consent_1 == "Yes" & consent_2 == "Yes")


## select the variable of interest of the descriptive statistics

pes_clean <- pes %>% dplyr::select(contains(c("pes")), "Gender", "Faculty", "Program" , "Year Program", "First Semester", "PA_profile")


## REPLACE VALUES OF PES AND OF PA Profile

pes_clean[pes_clean=="1 - I completely disagree"] <- "1"
pes_clean[pes_clean=="2 - I disagree"] <- "2"
pes_clean[pes_clean=="3 - I neither agree nor disagree"] <- "3"
pes_clean[pes_clean=="4 - I agree"] <- "4"
pes_clean[pes_clean=="5 - I completely agree"] <- "5"

pes_clean["PA_profile"][pes_clean["PA_profile"] == "Regular hard physical training for competition sports: running events, orienteering, skiing, swimming, soccer, or similar activities, several times per week."] <- "Vigorous PA"

pes_clean["PA_profile"][pes_clean["PA_profile"] == "Regular moderate physical activity and training: heavy gardening, running, swimming, fitness, tennis, badminton, or similar activities, for at least 2 to 3 hours by week."] <- "Moderate PA"

pes_clean["PA_profile"][pes_clean["PA_profile"] == "Sedentary: being almost completely inactive: reading, TV watching, movies, using computers or doing other sedentary activities during leisure time."] <- "Inactive"

pes_clean["PA_profile"][pes_clean["PA_profile"] == "Some light physical activity: riding a bicycle, walking with the family, gardening, table tennis, bowling, or similar activities, for at least 4 hours by week."] <- "Light PA"

table(pes_clean$PA_profile)


## Descriptive statistics to check if it matches statistics in the paper

table(pes_clean$Gender)
table(pes_clean$Faculty)
table(pes_clean$`Year Program`)


### REMOVE ALL INCOMPLETE DATA ON PES

pes_na <- na.omit(pes_clean)


write_csv(pes_na, "sport_psychology/Cheval_2_2024/dataset.csv")
