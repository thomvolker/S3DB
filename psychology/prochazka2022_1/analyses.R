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
  INPUT_FILE  <- "psychology/prochazka2022_1/dataset.csv"
  OUTPUT_FILE <- "psychology/prochazka2022_1/results/original.csv"
}

# data loading & results extraction packages
library(tidyverse)
library(broom)
library(broom.mixed)

# analysis package
library(lme4)

# read the data
dataset <- read_csv(INPUT_FILE)

# fit the models from the paper - study 1
fit1 <- t.test(task_intense ~ condition, data = dataset, var.equal = TRUE)
fit2 <- t.test(task_unpleasant ~ condition, data = dataset, var.equal = TRUE)
fit3 <- lm(cooperation ~ condition, data = dataset)

# two one-sided t-tests for equivalence testing
n         <- table(dataset$condition)
variances <- sapply(split(dataset$cooperation, dataset$condition), var)
pooledsd  <- sqrt(sum((n-1) * variances) / (sum(n) - 2))
coop      <- dataset$cooperation - mean(dataset$cooperation[dataset$condition == 0])
coop      <- coop / pooledsd

fit4 <- t.test(coop ~ dataset$condition,
               mu = -.39, alternative = "greater")
fit5 <- t.test(coop ~ dataset$condition,
               mu = .39, alternative = "less")

# hypothesis testing with multilevel models
fit6 <- lmer(cooperation ~ (1 | groupnr), data = dataset, REML = FALSE)
fit7 <- lmer(cooperation ~ condition + (1 | groupnr), data = dataset, REML = FALSE)
fit8 <- anova(fit6, fit7)


# extract the results and create output table
results <- bind_rows(
  t.test1     = tidy(fit1),
  t.test2     = tidy(fit2),
  anova       = tidy(fit3, conf.int = TRUE),
  eq_test1    = tidy(fit4),
  eq_test2    = tidy(fit5),
  mixed1      = tidy(fit6, conf.int = TRUE),
  mixed2      = tidy(fit7, conf.int = TRUE),
  mixed_comp1 = tidy(fit8),
  .id = "model" 
) |> select(model, term, estimate, std.error, conf.low, conf.high, statistic, logLik, df, parameter)

# store the results
write_csv(results, OUTPUT_FILE)


