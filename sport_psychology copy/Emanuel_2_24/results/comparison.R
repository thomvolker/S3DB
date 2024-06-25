library(tidyverse)

#load datasets

bstrap <- read_csv("sport_psychology/Emanuel_2_24/results/bootstrap_sample.csv")
bstrap_ind <- read_csv("sport_psychology/Emanuel_2_24/results/bootstrap_independent.csv")
spop_para <- read_csv("sport_psychology/Emanuel_2_24/results/synthpop_parametric.csv")
spop <- read_csv("sport_psychology/Emanuel_2_24/results/synthpop_cart.csv")
original <- read_csv("sport_psychology/Emanuel_2_24/results/original.csv")

#remove unnecessary columns
#original <- subset(original, select = -c(model, term, op, parameter, method, alternative))
#bstrap <- subset(bstrap, select = -c(model, term, op, parameter, method, alternative))
#bstrap_ind <- subset(bstrap_ind, select = -c(model, term, op, parameter, method, alternative))
#spop <- subset(spop, select = -c(model, term, op, parameter, method, alternative))
#spop_para <- subset(spop_para, select = -c(model, term, op, parameter, method, alternative))

original = as.data.frame(original)
bstrap = as.data.frame(bstrap)
bstrap_ind = as.data.frame(bstrap_ind)
spop = as.data.frame(spop)
spop_para = as.data.frame(spop_para)

#Create Functions with eval techniques
MSE <- function(original_data, synth_data) {
  test <- original_data$estimate - synth_data$estimate
  test <- test / original_data$std.error
  test <- test^2
  test[test == Inf] <- NA
  test <- mean(test, na.rm = TRUE)
}

ci_overlap <- function(original_data, synth_data) {
  width_overlap <- min(original_data$conf.high, synth_data$conf.high, na.rm = TRUE) - max(original_data$conf.low, synth_data$conf.low, na.rm = TRUE) 
  width_new <- synth_data$conf.high - synth_data$conf.low
  width_orig <- original_data$conf.high - original_data$conf.low
  mean(2*width_overlap / (width_new + width_orig), na.rm = TRUE)
}

#run the functions
bstrap_mse <- MSE(original, bstrap)
bstrap_ind_mse <- MSE(original, bstrap_ind)
synthpop_mse <- MSE(original, spop)
synthpop_para_mse <- MSE(original, spop_para)

bstrap_ci <- ci_overlap(original, bstrap)
bstrap_ind_ci <- ci_overlap(original, bstrap_ind)
synthpop_ci <- ci_overlap(original, spop)
synthpop_para_ci <- ci_overlap(original, spop_para)

#Create dataframes with the outputs of the functions
mse_dataframe <- rbind(bstrap_mse, bstrap_ind_mse, synthpop_mse, synthpop_para_mse)
mse_dataframe <- as.data.frame(mse_dataframe)

ci_dataframe <- rbind(bstrap_ci, bstrap_ind_ci, synthpop_ci, synthpop_para_ci)
ci_dataframe <- as.data.frame(ci_dataframe)

#plot MSE
theme_update(plot.title = element_text(hjust = 0.5))
gen <- c("Bootstrap", "Bootstrap_ind", "Synthpop_def", "Synthpop_para")

ggplot(mse_dataframe, aes(x = gen, y = V1)) +
  geom_bar(stat = "identity", fill = "cornflowerblue") +
  labs(title = "MSE Scores", x = "Generation Techniques", y = "Score") +
  theme(axis.line = element_line(linewidth = 1, colour = "black", linetype=1),
        axis.ticks = element_line(linewidth = 1, colour = "black"),
        axis.ticks.length = unit(.2, "cm"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

#plot CI
ggplot(ci_dataframe, aes(x = gen, y = V1)) +
  geom_bar(stat = "identity", fill = "cornflowerblue") +
  labs(title = "CI Scores", x = "Generation Techniques", y = "Score") +
  theme(axis.line = element_line(linewidth = 1, colour = "black", linetype=1),
        axis.ticks = element_line(linewidth = 1, colour = "black"),
        axis.ticks.length = unit(.2, "cm"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
