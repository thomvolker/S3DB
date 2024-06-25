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
  INPUT_FILE  <- "sport_psychology/Theobald_2022/synthetic_data/bootstrap_sample.csv"
  OUTPUT_FILE <- "sport_psychology/Theobald_2022/results/bootstrap_sample.csv"
}

# data loading & results extraction packages
library(tidyverse)
library(broom)
library(broom.mixed)

# Load necessary packages


# read the data
dataset <- read_csv(INPUT_FILE)


fit1 <- lm(performance_exam~ test_anxiety, data = dataset)

fit2 <- lm(performance_exam~ test_anxiety+performance_mock_exams, data = dataset)

fit3 <- lm(performance_exam~ test_anxiety*working_memory_trait+performance_mock_exams, data = dataset)

fit4 <- lm(performance_exam~ test_anxiety+performance_exam_prepare, data = dataset)

fit5 <- t.test(test_anxiety~ cg, data = dataset, var.equal=T)
fit6 <- t.test(performance_exam~ cg, data = dataset, var.equal=T)
fit7 <- t.test(performance_mock_exams~ cg, data = dataset, var.equal=T)
fit8 <- t.test(performance_exam_prepare~ cg, data = dataset, var.equal=T)

# extract the results and create output table
results <- bind_rows(
  lm1 = tidy(fit1, conf.int = TRUE),
  lm2 = tidy(fit2, conf.int = TRUE),
  lm3 = tidy(fit3, conf.int = TRUE),
  lm4 = tidy(fit4, conf.int = TRUE),
  t.test1     = tidy(fit5),
  t.test2     = tidy(fit6),
  t.test3     = tidy(fit7),
  t.test4     = tidy(fit8),
  .id = "model" 
) |> dplyr::select(model, term, estimate, std.error, conf.low, conf.high, statistic)


# store the results
write_csv(results, OUTPUT_FILE)

