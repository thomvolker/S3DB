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
  INPUT_FILE  <- "psychology/prochazka2022_2/dataset.csv"
  OUTPUT_FILE <- "psychology/prochazka2022_2/results/original.csv"
}

# data loading & results extraction packages
library(tidyverse)
library(broom)
library(broom.mixed)

# analysis package
library(rstatix)
library(lme4)

# read the data
dataset <- read_csv(INPUT_FILE)

# fit the models from the paper - study 1
fit1 <- t.test(task_intense ~ condition, data = dataset)
fit2 <- t.test(task_unpleasant ~ condition, data = dataset)
fit3 <- t.test(betterperf ~ condition, data = dataset)
fit4 <- t.test(taskascompetition ~ condition, data = dataset)
fit5 <- lm(Lottery_TOT ~ condition, data = dataset)

# two one-sided t-tests for equivalence testing
n         <- table(dataset$condition)
variances <- sapply(split(dataset$Lottery_TOT, dataset$condition), var)
pooledsd  <- sqrt(sum((n-1) * variances) / (sum(n) - 2))
coop      <- dataset$Lottery_TOT - mean(dataset$Lottery_TOT[dataset$condition == 0])
coop      <- coop / pooledsd

fit6 <- t.test(coop ~ dataset$condition,
               mu = -.39, alternative = "greater")
fit7 <- t.test(coop ~ dataset$condition,
               mu = .39, alternative = "less")

# hypothesis testing with multilevel models
fit8  <- lmer(Lottery_TOT ~ (1 | groupnr), data = dataset, REML = FALSE)
fit9  <- lmer(Lottery_TOT ~ condition + (1 | groupnr), data = dataset, REML = FALSE)
fit10 <- anova(fit8, fit9)

# non-preregistered analyses
fit11 <- lmer(Lottery_TOT ~ condition + taskascompetition + (1 | groupnr),
              data = dataset, REML = FALSE)
fit12 <- lmer(Lottery_TOT ~ factor(condition)*taskascompetition + (1 | groupnr),
              data = dataset, REML = FALSE)
fit13 <- anova(fit11, fit9)
fit14 <- anova(fit12, fit11)

# extract the results and create output table
results <- bind_rows(
  t.test1     = tidy(fit1),
  t.test2     = tidy(fit2),
  t.test3     = tidy(fit3),
  t.test4     = tidy(fit4),
  anova       = tidy(fit5, conf.int = TRUE),
  eq_test1    = tidy(fit6),
  eq_test2    = tidy(fit7),
  mixed1      = tidy(fit8, conf.int = TRUE),
  mixed2      = tidy(fit9, conf.int = TRUE),
  mixed_comp1 = tidy(fit10),
  mixed3      = tidy(fit11, conf.int = TRUE),
  mixed4      = tidy(fit12, conf.int = TRUE),
  mixed_comp2 = tidy(fit13),
  mixed_comp3 = tidy(fit14),
  .id = "model" 
) |> select(model, term, estimate, std.error, conf.low, conf.high, statistic, logLik, df, parameter)

# store the results
write_csv(results, OUTPUT_FILE)
