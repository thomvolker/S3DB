# Load necessary library 
library(ggplot2)

# Step 1: Create mock rough oval-shaped data rotated by 45 degrees
set.seed(123)
n_points <- 100

# Create an elliptical distribution with additional noise
theta <- runif(n_points, 0, 2 * pi)
a <- 7  # semi-major axis
b <- 3  # semi-minor axis
r <- 5 + rnorm(n_points, mean = 0, sd = 0.5)  # radius with some noise

# Generate elliptical points with additional randomness
x_ellipse <- a * cos(theta) + rnorm(n_points, sd = 0.5)
y_ellipse <- b * sin(theta) + rnorm(n_points, sd = 0.5)

# Rotate ellipse by 45 degrees
angle <- pi / 4
x_rot <- x_ellipse * cos(angle) - y_ellipse * sin(angle)
y_rot <- x_ellipse * sin(angle) + y_ellipse * cos(angle)

# Shift the center to (50, 100)
x <- x_rot + 50
y <- y_rot + 100

original_data <- data.frame(
  x = x,
  y = y,
  group = 'Original'
)


# Create a bootstrap sample with added variability
set.seed(123)
n <- nrow(original_data)
bootstrap_sample_indices <- sample(1:n, size = n, replace = TRUE)
bootstrap_sample <- original_data[bootstrap_sample_indices, ]
bootstrap_sample$y <- bootstrap_sample$y + rnorm(n, mean = 0, sd = 0.1)  # Add more noise
bootstrap_sample$group <- 'Bootstrap'

# Combine datasets
combined_data <- rbind(original_data, bootstrap_sample)

# Step 3: Visualize combined data as a dot plot with transparency
ggplot(combined_data, aes(x = x, y = y, color = group)) +
  geom_point(alpha = 0.4, size = 2) +
  ggtitle("Original Data and Bootstrap Sample Dot Plot") +
  theme_minimal() +
  scale_color_manual(values = c("Original" = "deeppink", "Bootstrap" = "darkorange"))












# Load necessary library
library(ggplot2)

# Step 1: Create mock rough oval-shaped data rotated by 45 degrees
set.seed(123)
n_points <- 100

# Create an elliptical distribution with additional noise
theta <- runif(n_points, 0, 2 * pi)
a <- 7  # semi-major axis
b <- 3  # semi-minor axis
r <- 5 + rnorm(n_points, mean = 0, sd = 0.5)  # radius with some noise

# Generate elliptical points with additional randomness
x_ellipse <- a * cos(theta) + rnorm(n_points, sd = 0.5)
y_ellipse <- b * sin(theta) + rnorm(n_points, sd = 0.5)

# Rotate ellipse by 45 degrees
angle <- pi / 4
x_rot <- x_ellipse * cos(angle) - y_ellipse * sin(angle)
y_rot <- x_ellipse * sin(angle) + y_ellipse * cos(angle)

# Shift the center to (50, 100)
x <- x_rot + 50
y <- y_rot + 100

original_data <- data.frame(
  x = x,
  y = y,
  group = 'Original'
)

# Step 2: Create a bootstrap sample
set.seed(123)
n <- nrow(original_data)
bootstrap_sample <- original_data[sample(1:n, size = n, replace = TRUE), ]
bootstrap_sample$group <- 'Bootstrap'

# Step 3: Fit a linear model to the original data
fit_model <- lm(y ~ x, data = original_data)

# Step 4: Predict values for the bootstrap sample using the fitted model
bootstrap_sample$y_pred <- predict(fit_model, newdata = bootstrap_sample)

# Step 5: Visualize the original data, bootstrap sample, and fitted model
ggplot() +
  geom_point(data = original_data, aes(x = x, y = y), color = "#FF6EC7", alpha = 0.5, size = 2) +
  geom_line(data = bootstrap_sample, aes(x = x, y = y_pred), color = "#FF4500", size = 1) +
  geom_point(data = bootstrap_sample, aes(x = x, y = y), color = "#FF4500", alpha = 0.5, size = 2) +
  ggtitle("Bootstrap Sampling and Model Fitting Illustration") +
  theme_minimal() +
  labs(color = "Dataset") +
  scale_color_manual(values = c("Original" = "#FF6EC7", "Bootstrap" = "#FF4500"))











######################




# Load necessary libraries
library(ggplot2)

# Set seed for reproducibility
set.seed(0)

# Generate original data
n_points <- 100
theta <- seq(0, 2 * pi, length.out = n_points)
x <- 10 * cos(theta) + rnorm(n_points)
y <- 10 * sin(theta) + rnorm(n_points)
original_data <- data.frame(x = x, y = y)

# Generate synthetic data
set.seed(1)
x_synthetic <- 10 * cos(theta) + rnorm(n_points)
y_synthetic <- 10 * sin(theta) + rnorm(n_points)
synthetic_data <- data.frame(x = x_synthetic, y = y_synthetic)

# Create plots
p1 <- ggplot(original_data, aes(x, y)) +
  geom_point() +
  ggtitle("Original data") +
  theme_minimal()

p2 <- ggplot(synthetic_data, aes(x, y)) +
  geom_point() +
  ggtitle("Synthetic data") +
  theme_minimal()

# Combine plots
library(gridExtra)
grid.arrange(p1, p2, ncol = 2, top = "The synthetic data retains the structure of the original data but is not the same")






# Load necessary libraries
library(ggplot2)

# Set seed for reproducibility
set.seed(0)

# Generate original data
n_points <- 100
theta <- seq(0, 2 * pi, length.out = n_points)
x <- 10 * cos(theta) + rnorm(n_points)
y <- 10 * sin(theta) + rnorm(n_points)
original_data <- data.frame(x = x, y = y)

# Generate synthetic data
set.seed(1)
x_synthetic <- 10 * cos(theta) + rnorm(n_points)
y_synthetic <- 10 * sin(theta) + rnorm(n_points)
synthetic_data <- data.frame(x = x_synthetic, y = y_synthetic)

# Create plots
p1 <- ggplot(original_data, aes(x, y)) +
  geom_point(color = "blue") +
  ggtitle("Original data") +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

p2 <- ggplot(synthetic_data, aes(x, y)) +
  geom_point(color = "blue") +
  ggtitle("Synthetic data") +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

# Combine plots
library(gridExtra)
grid.arrange(p1, p2, ncol = 2, top = "The synthetic data retains the structure of the original data but is not the same")




# Load necessary libraries
library(ggplot2)

# Set seed for reproducibility
set.seed(0)

# Generate original data
n_points <- 100
theta <- seq(0, 2 * pi, length.out = n_points)
x <- 10 * cos(theta) + rnorm(n_points)
y <- 10 * sin(theta) + rnorm(n_points)
original_data <- data.frame(x = x, y = y)

# Generate synthetic data
set.seed(1)
x_synthetic <- 10 * cos(theta) + rnorm(n_points)
y_synthetic <- 10 * sin(theta) + rnorm(n_points)
synthetic_data <- data.frame(x = x_synthetic, y = y_synthetic)

# Create plots
p1 <- ggplot(original_data, aes(x, y)) +
  geom_point(color = "deeppink", size = 3) +
  ggtitle("") +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

p2 <- ggplot(synthetic_data, aes(x, y)) +
  geom_point(color = "darkorange", size = 3) +
  ggtitle("") +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

# Combine plots
library(gridExtra)
grid.arrange(p1, p2, ncol = 2)







# Load necessary libraries
library(ggplot2)

# Set seed for reproducibility
set.seed(0)

# Generate original data
n_points <- 100
theta <- seq(0, 2 * pi, length.out = n_points)
x <- 10 * cos(theta) + rnorm(n_points)
y <- 10 * sin(theta) + rnorm(n_points)
original_data <- data.frame(x = x, y = y)

# Generate synthetic data
set.seed(1)
x_synthetic <- 10 * cos(theta) + rnorm(n_points)
y_synthetic <- 10 * sin(theta) + rnorm(n_points)
synthetic_data <- data.frame(x = x_synthetic, y = y_synthetic)

# Create plots
p1 <- ggplot(original_data, aes(x, y)) +
  geom_point(color = "blue", size = 3) +
  ggtitle("Original data") +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA))

p2 <- ggplot(synthetic_data, aes(x, y)) +
  geom_point(color = "red", size = 3) +
  ggtitle("Synthetic data") +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA))

# Save plots with transparent background
ggsave("original_data_plot.png", plot = p1, bg = "transparent")
ggsave("synthetic_data_plot.png", plot = p2, bg = "transparent")

# Combine plots
library(gridExtra)
combined_plot <- grid.arrange(p1, p2, ncol = 2, top = "The synthetic data retains the structure of the original data but is not the same")

# Save combined plot
ggsave("combined_plot.png", plot = combined_plot, bg = "transparent", width = 6, height = 3)







# Load necessary libraries
library(ggplot2)

# Set seed for reproducibility
set.seed(0)

# Generate original data
n_points <- 100
theta <- seq(0, 2 * pi, length.out = n_points)
x <- 10 * cos(theta) + rnorm(n_points)
y <- 10 * sin(theta) + rnorm(n_points)
original_data <- data.frame(x = x, y = y)

# Generate synthetic data
set.seed(1)
x_synthetic <- 10 * cos(theta) + rnorm(n_points)
y_synthetic <- 10 * sin(theta) + rnorm(n_points)
synthetic_data <- data.frame(x = x_synthetic, y = y_synthetic)

# Create plots
p1 <- ggplot(original_data, aes(x, y)) +
  geom_point(color = "#8F2D56", size = 3) +
  ggtitle("Original data") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA)
  )

p2 <- ggplot(synthetic_data, aes(x, y)) +
  geom_point(color = "#D81159", size = 3) +
  ggtitle("Synthetic data") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA)
  )

# Save plots with transparent background
ggsave("original_data_plot.png", plot = p1, bg = "transparent", width = 6, height = 3)
ggsave("synthetic_data_plot.png", plot = p2, bg = "transparent", width = 6, height = 3)

# Combine plots
library(gridExtra)
combined_plot <- grid.arrange(p1, p2, ncol = 2, top = "The synthetic data retains the structure of the original data but is not the same")

# Save combined plot
ggsave("combined_plot.png", plot = combined_plot, bg = "transparent", width = 6, height = 3)



library(ggplot2)
library(gridExtra)
library(grid)
library(extrafont)  # Needed to use custom fonts

# Load fonts (only necessary once per session)
loadfonts(device = "mac")  # or use "mac" or "pdf" depending on your platform

# Create individual plots
p1 <- ggplot(original_data, aes(x, y)) +
  geom_point(color = "#8F2D56", size = 3) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text(color = "black", family = "Aptos"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    plot.margin = margin(t = 0, r = 20, b = 0, l = 0)
  )

p2 <- ggplot(synthetic_data, aes(x, y)) +
  geom_point(color = "#D81159", size = 3) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text(color = "black", family = "Aptos"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 20)
  )

# Add annotations
annotation_grob1 <- textGrob("Original data", gp = gpar(fontfamily = "Aptos", fontsize = 12, col = "black"))
annotation_grob2 <- textGrob("Synthetic data", gp = gpar(fontfamily = "Aptos", fontsize = 12, col = "black"))

# Arrange plots and annotations
combined_plot <- grid.arrange(
  arrangeGrob(p1, p2, ncol = 2, widths = c(3, 3)),
  bottom = arrangeGrob(annotation_grob1, annotation_grob2, ncol = 2)
)

# Save combined plot
ggsave("combined_plot.png", plot = combined_plot, bg = "transparent", width = 6, height = 3)
       



library(ggplot2)
library(grid)
library(extrafont)  # Needed to use custom fonts

# Load fonts (only necessary once per session)
loadfonts(device = "mac")  # or use "mac" or "pdf" depending on your platform

# Create individual plots with annotations
p1 <- ggplot(original_data, aes(x, y)) +
  geom_point(color = "#8F2D56", size = 3) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text(color = "black", family = "Aptos"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    plot.margin = margin(t = 0, r = 20, b = 20, l = 0)
  ) +
  annotate("text", x = Inf, y = -Inf, label = "Original data", hjust = 1.5, vjust = -1.5, color = "black", family = "Aptos")

p2 <- ggplot(synthetic_data, aes(x, y)) +
  geom_point(color = "#D81159", size = 3) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text(color = "black", family = "Aptos"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    plot.margin = margin(t = 0, r = 0, b = 20, l = 20)
  ) +
  annotate("text", x = Inf, y = -Inf, label = "Synthetic data", hjust = 1.5, vjust = -1.5, color = "black", family = "Aptos")

# Save the individual plots with annotations
ggsave("original_data_plot_annotated.png", plot = p1, bg = "transparent", width = 6, height = 3)
ggsave("synthetic_data_plot_annotated.png", plot = p2, bg = "transparent", width = 6, height = 3)

# Create annotations for combining
annotation_grob1 <- textGrob("Original data", gp = gpar(fontfamily = "Aptos", fontsize = 12, col = "black"))
annotation_grob2 <- textGrob("Synthetic data", gp = gpar(fontfamily = "Aptos", fontsize = 12, col = "black"))

# Combine plots and annotations
combined_plot <- grid.arrange(
  arrangeGrob(p1, p2, ncol = 2, widths = c(1, 1)),
  bottom = arrangeGrob(annotation_grob1, annotation_grob2, ncol = 2)
)

# Save the combined plot
ggsave("combined_plot_annotated.png", plot = combined_plot, bg = "transparent", width = 12, height = 6)






