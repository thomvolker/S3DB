# Load necessary libraries
library(dplyr)
library(readr)
library(synthpop)

# Read the original data
original_data <- read.csv("sport_psychology/Emanuel_2_24/dataset.csv")
view(original_data)

### Bootstrap sample
set.seed(123) 
n <- nrow(original_data)
bootstrap_sample <- original_data[sample(1:n, size = n, replace = TRUE), ]
view(bootstrap_sample)

# Save the synthetic data to a new CSV file
write.csv(bootstrap_sample, "sport_psychology/Emanuel_2_24/synthetic_data/bootstrap_sample.csv", row.names = FALSE)


### Bootstrap samples with each variable independently
set.seed(123) # For reproducibility
n <- nrow(original_data)
bootstrap_independent <- as.data.frame(lapply(original_data, function(col) {
  sample(col, size = n, replace = TRUE)
}))

# Save the synthetic data to a new CSV file
write.csv(bootstrap_independent, "sport_psychology/Emanuel_2_24/synthetic_data/bootstrap_independent.csv", row.names = FALSE)


### Synthpop parametric
set.seed(123)
synth_data <- syn(original_data, method = "parametric")

synthpop_parametric <- synth_data$syn
write.csv(synthpop_parametric, "sport_psychology/Emanuel_2_24/synthetic_data/synthpop_parametric.csv", row.names = FALSE)

### Synthpop "cart"
set.seed(123)
synth_data_cart <- syn(original_data, method = "cart") 
synthpop_cart <- synth_data_cart$syn
write.csv(synthpop_cart, "sport_psychology/Emanuel_2_24/synthetic_data/syntpop_cart.csv", row.names = FALSE)


