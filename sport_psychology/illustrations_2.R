
#########




# Load necessary library
library(ggplot2)
library(cowplot)

# Create original S-shaped data using the logistic function
set.seed(123)  # for reproducibility
x <- seq(-10, 10, length.out = 100)
y <- 1 / (1 + exp(-x))

# Add more jitter to the data points to make it more scattered
y_jittered <- y + rnorm(100, mean = 0, sd = 0.05)

# Create the original data frame
original_data <- data.frame(x = x, y = y_jittered)
original_data$group <- 'Original'

# Create a bootstrap sample with added variability
set.seed(123)
n <- nrow(original_data)
bootstrap_sample_indices <- sample(1:n, size = n, replace = TRUE)
bootstrap_sample <- original_data[bootstrap_sample_indices, ]
bootstrap_sample$y <- bootstrap_sample$y + rnorm(n, mean = 0, sd = 0.01)  # Add more noise
bootstrap_sample$group <- 'Bootstrap'
# Combine the original data and bootstrap sample
combined_data <- rbind(original_data, bootstrap_sample)

# First plot: Dot plot of original data (pink dots)
plot1 <- ggplot(original_data, aes(x = x, y = y)) +
  geom_point(color = "deeppink", size = 2, alpha = 0.6) +
  labs(title = "Original Data", x = "X", y = "Y") +
  theme_void()

# Second plot: Dot plot of original data with less fitted best fit line
plot2 <- ggplot(original_data, aes(x = x, y = y)) +
  geom_point(color = "deeppink", size = 2, alpha = 0.6) +
  geom_smooth(method = "loess", span = 1.1, color = "darkorange", size = 1, se = FALSE) + # Less fitted with smaller span
  labs(title = "Fit model to original data", x = "X", y = "Y") +
  theme_void()

# # Third plot: Dot plot with original and synthetic data
# plot3 <- ggplot(combined_data, aes(x = x, y = y, color = group)) +
#   geom_point(aes(alpha = group), size = 2) +  # Adjust transparency by group
#   scale_alpha_manual(values = c("Original" = 0.4, "Bootstrap" = 0.8)) +  # Set different alphas
#   labs(title = "Generate new data from fitted model's distribution", x = "X", y = "Y") +
#   theme_minimal() +
#   scale_color_manual(values = c("Original" = "deeppink", "Bootstrap" = "darkorange")) +
#   theme_void()

# Third plot: Dot plot with original and synthetic data without combining
plot3 <- ggplot() +
  geom_point(data = original_data, aes(x = x, y = y, color = 'Original data'), size = 2, alpha = 0.4) +
  geom_point(data = bootstrap_sample, aes(x = x, y = y, color = 'Synthetic data'), size = 2, alpha = 0.8) +
  labs(title = "Generate new data from fitted model's distribution", x = "X", y = "Y") +
  theme_minimal() +
  scale_color_manual(values = c("Original data" = "deeppink", "Synthetic data" = "darkorange")) +
  theme_void() 


# # Save the plots
# ggsave("plot1.png", plot = plot1, width = 8, height = 6)
# ggsave("plot2.png", plot = plot2, width = 8, height = 6)
# ggsave("plot3.png", plot = plot3, width = 8, height = 6)

# Display the plots
print(plot1)
print(plot2)
print(plot3)


# Combine the plots into one image
combined_plot <- plot_grid(plot1, plot2, plot3, ncol = 3)
print(combined_plot)















