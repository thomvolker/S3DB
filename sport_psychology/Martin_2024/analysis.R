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
  INPUT_FILE  <- "sport_psychology/Martin_2024/dataset.csv"
  OUTPUT_FILE <- "sport_psychology/Martin_2024/results/original.csv"
}

# data loading & results extraction packages
library(tidyverse)
library(broom)
library(broom.mixed)

# analysis packages
library(car)    
library(lme4) 

# read the data
dataset <- read_csv(INPUT_FILE)

# fit the models from the paper 
fit1 <- glmer(homo ~ 1 + populationC + (1|sujet), data=dataset, family=binomial)
fit2 <- glmer(homo ~ 1 + (1|sujet), data=dataset, family=binomial)
fit3 <- anova(fit1, fit2)


# extract the results and create output table
results <- bind_rows(
  anova      = tidy(fit3, conf.int = TRUE),
  glmer = tidy(fit1, conf.int = TRUE),
  nullmodel = tidy(fit2, conf.int = TRUE),
  .id = "model" 
) |> dplyr::select(model, term, estimate, std.error, conf.low, conf.high, statistic)

# store the results
write_csv(results, OUTPUT_FILE)

