# set input file and output file
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 1) stop("Both input and output file are required!")
  if (length(args) == 0) {
    INPUT_FILE  <- "dataset.csv"
    OUTPUT_FILE <- "results/original.csv"
  } else {
    INPUT_FILE  <- args[1]
    OUTPUT_FILE <- args[2]
  }
} else {
  INPUT_FILE  <- "sport_psychology/Chen_1_2024/dataset.csv"
  OUTPUT_FILE <- "sport_psychology/Chen_1_2024/results/original.csv"
}

# data loading & results extraction packages
library(tidyverse)
library(broom)
library(broom.mixed)

# analysis package
library(lme4)

# read the data
dataset <- read_csv(INPUT_FILE)

# fit the models from the paper
fit1 <- lm(highschoolGPA ~ group, dataset)
fit2 <- lm(pre_intervention_GPA ~ group, dataset)

fit3 <- lm(gradegoal ~ group, dataset)
fit4 <- lm(gradegoal_E2 ~ group, dataset)

fit5 <- lm(motivation ~ group, dataset)
fit6 <- lm(motivation_E2 ~ group, dataset)

fit7 <- lm(importance ~ group, dataset)
fit8 <- lm(importance_E2 ~ group, dataset)

fit9 <- lm(confidence ~ group, dataset)
fit10 <- lm(confidence_E2 ~ group, dataset)

fit11 <- t.test(dataset$grade~dataset$group)
fit12 <- t.test(dataset$exam_1~dataset$group)
fit13 <- t.test(dataset$exam_2~dataset$group)



# extract the results and create output table
results <- bind_rows(
  lm1 = tidy(fit1, conf.int = TRUE),
  lm2 = tidy(fit2, conf.int = TRUE),
  lm3 = tidy(fit3, conf.int = TRUE),
  lm4 = tidy(fit4, conf.int = TRUE),
  lm5 = tidy(fit5, conf.int = TRUE),
  lm6 = tidy(fit6, conf.int = TRUE),
  lm7 = tidy(fit7, conf.int = TRUE),
  lm8 = tidy(fit8, conf.int = TRUE),
  lm9 = tidy(fit9, conf.int = TRUE),
  lm10 = tidy(fit10, conf.int = TRUE),
  t.test1 = tidy(fit11),
  t.test2 = tidy(fit12),
  t.test3 = tidy(fit13),
  .id = "model" 
) |> dplyr::select(model, term, estimate, std.error, conf.low, conf.high, statistic)

# store the results
write_csv(results, OUTPUT_FILE)

