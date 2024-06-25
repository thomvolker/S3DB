library(haven)
library(readr)

dataset <- read_csv("sport_psychology/Chen_1_2024/preproc/study1_analysis_release.csv")


dataset <- dataset |>
  dplyr::select(highschoolGPA, pre_intervention_GPA, gradegoal, gradegoal_E2, 
         motivation, motivation_E2, importance, importance_E2, 
         confidence, confidence_E2, grade, exam_1, exam_2, group)

write_csv(dataset, "sport_psychology/Chen_1_2024/dataset.csv")

