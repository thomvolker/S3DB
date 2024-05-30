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
  INPUT_FILE  <- "sport_psychology/Ziv_2024/synthetic_data/synthpop_default.csv"
  OUTPUT_FILE <- "sport_psychology/Ziv_2024/results/synthpop_default.csv"
}

# data loading & results extraction packages
library(tidyverse)
library(broom)
library(broom.mixed)

# Load necessary packages
library(car)      # for the ANOVA function
library(emmeans)  # for post-hoc analysis

# read the data
dataset <- read_csv(INPUT_FILE)

## fit the models from the paper

# The one-way repeated-measures ANOVA
anova <- aov(score ~ belief_type + Error(id/belief_type), data = dataset)
summary(anova)

# Post-hoc analysis using Holm-Bonferroni method
pairwise_holm <- pairwise.t.test(dataset$score, dataset$belief_type, p.adjust.method = "holm")
print(pairwise_holm)

# The Friedman test (non-parametric equivalent of one-way reapeated-measure ANOVA)
friedman <- friedman.test(score ~ belief_type | id, data = dataset)
summary(friedman)


# extract the results and create output table
results <- bind_rows(
  anova = tidy(anova, conf.int = TRUE),
  pairwise = tidy(pairwise_holm),
  friedman = tidy(friedman),
  .id = "model" 
) |> select(model, term)

# store the results
write_csv(results, OUTPUT_FILE)

