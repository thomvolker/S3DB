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
  INPUT_FILE  <- "sport_psychology/St_Cyr_1_2024/synthetic_data/bootstrap_sample.csv"
  OUTPUT_FILE <- "sport_psychology/St_Cyr_1_2024/results/bootstrap_sample.csv"
  
}

# data loading & results extraction packages
library(tidyverse)
library(broom)
library(broom.mixed)


# analysis package
library(tidyverse)
library(lavaan)
library(semPlot)
library(semTools)
library(Hmisc)

# read the data
dataset <- read_csv(INPUT_FILE)

#   Variables                                                               ####

#' Variables used in the path analysis
#' SOP: self-oriented perfectionism
#' SPP: socially prescribed perfectionism
#' HP: Harmonious passion
#' OP: Obsessive passion
#' eat_disorder: disordered eating
#' psy_wb: psychological well-being
#' PHYS_H: physical well-being 

#   ____________________________________________________________________________
#   Path Analysis                                                           ####

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Path Analysis 1                                                        ####

hypes <- '
# Structural model
HP ~ sohp*SOP + sphp*SPP
OP ~ soop*SOP + spop*SPP
PHYS_H ~ hph*HP + oph*OP
psy_wb ~ hpwb*HP + opwb*OP
eat_disorder ~ hpeat*HP + opeat*OP
SOP ~~ SPP
HP ~~ OP
PHYS_H ~~ psy_wb ; psy_wb ~~ eat_disorder ; eat_disorder ~~ PHYS_H

# Indirect effects
ind_sohp_hpwb := sohp*hpwb
ind_sohp_hpeat := sohp*hpeat
ind_sohp_hph := sohp*hph

ind_soop_opwb := soop*opwb
ind_soop_opeat := soop*opeat
ind_soop_oph := soop*oph

ind_sphp_hpwb := sphp*hpwb
ind_sphp_hpeat := sphp*hpeat
ind_sphp_hph := sphp*hph

ind_spop_opwb := spop*opwb
ind_spop_opeat := spop*opeat
ind_spop_oph := spop*oph

indtot := sohp*hpwb + sohp*hpeat + sohp*hph + soop*opwb + soop*opeat + soop*oph + sphp*hpwb + sphp*hpeat + sphp*hph + spop*opwb + spop*opeat + spop*oph
'

#' Results path analysis 1
modPath1 <- sem(hypes, data=dataset, estimator="MLR")

summary(modPath1, fit.measures=T, standardized=T, rsquare=T, ci=T)

#' Indirect Effects
mod1boot <- sem(modPath1, data=dataset, estimator="ML", se="bootstrap")

summary(mod1boot, fit.measures=T, standardized=T, rsquare=T, ci=T)

#' Non significant paths will be removed to have a more parsimonious model.
#' We removed: 
#' HP - SPP
#' PHYS_H - OP
#' psy_wb - OP
#' eat_disorder - HP

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Path Analysis 2                                                        ####

hypes2 <- '
# Structural model
HP ~ sohp*SOP
OP ~ soop*SOP + spop*SPP
PHYS_H ~ hph*HP
psy_wb ~ hpwb*HP
eat_disorder ~ opeat*OP
SOP ~~ SPP
HP ~~ OP
PHYS_H ~~ psy_wb ; psy_wb ~~ eat_disorder ; eat_disorder ~~ PHYS_H

# Indirect effects
ind_sohp_hpwb := sohp*hpwb

ind_sohp_hph := sohp*hph

ind_soop_opeat := soop*opeat

ind_spop_opeat := spop*opeat

indtot := sohp*hpwb + sohp*hph + soop*opeat + spop*opeat
'

#' Results path analysis 2
modPath2 <- sem(hypes2, data=dataset, estimator="MLR")

summary(modPath2, fit.measures=T, standardized=T, rsquare=T, ci=T)

#' Indirect Effects
mod2boot <- sem(modPath2, data=dataset, estimator="ML", se="bootstrap")

summary(mod2boot, fit.measures=T, standardized=T, rsquare=T, ci=T)


# extract the results and create output table
results <- bind_rows(
  path1    = tidy(modPath1, conf.int = TRUE),
  path2    = tidy(modPath2, conf.int = TRUE),
  boot1    = tidy(mod1boot, conf.int = TRUE),
  boot2    = tidy(mod2boot, conf.int = TRUE),
  .id = "model" 
) |> dplyr::select(model, term, estimate, std.error, conf.low, conf.high, statistic)

# store the results
write_csv(results, OUTPUT_FILE)

