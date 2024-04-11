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
  INPUT_FILE  <- "psychology/prochazka2022/dataset.csv"
  OUTPUT_FILE <- "psychology/prochazka2022/results/original.csv"
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
fit1 <- t.test(task_intense ~ condition, data = dataset, var.equal = TRUE)
fit2 <- lm(cooperation ~ condition, data = dataset)
fit3 <- lmer(cooperation ~ (1 | groupnr), data = dataset, REML = FALSE)
fit4 <- lmer(cooperation ~ condition + (1 | groupnr), data = dataset, REML = FALSE)


# extract the results and create output table
results <- bind_rows(
  ttest  = tidy(fit1),
  model1 = tidy(fit2, conf.int = TRUE),
  model2 = tidy(fit3, conf.int = TRUE),
  model3 = tidy(fit4, conf.int = TRUE),
  .id = "model" 
) |> select(model, term, estimate, std.error, conf.low, conf.high)

# store the results
write_csv(results, OUTPUT_FILE)


