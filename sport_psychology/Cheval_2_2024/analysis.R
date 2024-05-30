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
  INPUT_FILE  <- "sport_psychology/Cheval_2_2024/dataset.csv"
  OUTPUT_FILE <- "sport_psychology/Cheval_2_2024/results/original.csv"
}

# data loading & results extraction packages
library(tidyverse)
library(broom)
library(broom.mixed)

# analysis package
library(lme4)
library(gghalves)
library(hrbrthemes)
library(MetBrewer)


# read the data
dataset <- read_csv(INPUT_FILE)


## STEP 3: CFA analysis

pes_na <- dataset %>% mutate_at(c("PES_1", "PES_3", "PES_5","PES_7", "PES_9", "PES_11", "PES_13", "PES_15", "PES_18", "PES_2", "PES_4", "PES_6","PES_8", "PES_10", "PES_12", "PES_14", "PES_16", "PES_17"), as.numeric)



model_cfa1 <-'
approach =~ PES_3 + PES_7 + PES_18 + PES_15 
avoidance =~ PES_4 + PES_6 + PES_12 + PES_17

approach ~~ avoidance
'

fit<- lavaan::sem(model_cfa1,data=pes_na,orthogonal = TRUE)

summary(fit,fit.measures=TRUE,standardized=TRUE,rsquare=TRUE, ci = .95)


### PRELIMINARY CONSTRCUT VALIDITY: DESCRIBE BY PA PROFILE 



pes_na <- mutate(pes_na, pes_approach = (PES_3 + PES_7 + PES_15 + PES_18)/4)

pes_na <- mutate(pes_na, pes_avoid = (PES_4 + PES_6 + PES_12 + PES_17)/4)



# multiple linear regression

RLM_approach <- lm(pes_approach ~ PA_profile, data = pes_na)
summary(RLM_approach)



####

# extract the results and create output table
results <- bind_rows(
  CFA       = tidy(fit, conf.int = TRUE), #lavaan # Confirmatory factor analysis
  rlm       = tidy(RLM_approach, conf.int = TRUE), #multiple linear regression
  
  .id = "model" 
) |> select(model, term, estimate, std.error, conf.low, conf.high, statistic)

# store the results
write_csv(results, OUTPUT_FILE)
