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
  INPUT_FILE  <- "sport_psychology/Emanuel_2_24/synthetic_data/synthpop_cart.csv"
  OUTPUT_FILE <- "sport_psychology/Emanuel_2_24/results/synthpop_cart.csv"
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

# Center the 'repetition' variable
dataset$repetition_centered <- scale(dataset$repetition)

# Fit the linear mixed-effects regression model
model1 <- lmer(peak_force_percent ~ repetition_centered * condition + (1 + repetition + condition | sub_num), data = dataset)

# Display the summary of the model
summary(model1)

# Fit the quadratic mixed-effects model
model2 <- lmer(peak_force_percent ~ repetition_centered + I(repetition_centered^2) + condition + repetition_centered:condition + I(repetition_centered^2):condition + (1 + condition + repetition_centered + I(repetition_centered^2) | sub_num), data = dataset)

# Display the summary of the quadratic model
summary(model2)

# extract the results and create output table
results <- bind_rows(
  mixed1      = tidy(model1, conf.int = TRUE),
  quadrartic1 = tidy(model2, conf.int = TRUE),
  .id = "model" 
) |> select(model, term, estimate, std.error, conf.low, conf.high, statistic)

# store the results
write_csv(results, OUTPUT_FILE)

# Conditional R^2 values:

library(MuMIn)
# Compute conditional R^2 for the linear model
conditional_r_squared_linear <- r.squaredGLMM(model1)
conditional_r_squared_linear

# Compute conditional R^2 for the quadratic model
conditional_r_squared_quadratic <- r.squaredGLMM(model2)
conditional_r_squared_quadratic


