library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)

# read in result file
data <- read_excel("sport_psychology/fr_16_6_2.xlsx")


# Define the function for MSE
calculate_MSE <- function(data, study, method) {
  result <- data |>
    filter(STUDY == study, METHOD == method) |>
    summarise(mean_diff_squared = mean(DIFF_SQUARED, na.rm = TRUE))
  
  return(result$mean_diff_squared)
}

# Use function for all studies and methods

S1_MSE_boot <- calculate_MSE(data, "Study_1_Chen", "boot")
S1_MSE_independent <- calculate_MSE(data, "Study_1_Chen", "independent")
S1_MSE_parametric <- calculate_MSE(data, "Study_1_Chen", "parametric")
S1_MSE_cart <- calculate_MSE(data, "Study_1_Chen", "cart")

S2_MSE_boot <- calculate_MSE(data, "Study_2_Emanuel_1", "boot")
S2_MSE_independent <- calculate_MSE(data, "Study_2_Emanuel_1", "independent")
S2_MSE_parametric <- calculate_MSE(data, "Study_2_Emanuel_1", "parametric")
S2_MSE_cart <- calculate_MSE(data, "Study_2_Emanuel_1", "cart")

S3_MSE_boot <- calculate_MSE(data, "Study_3_Emanuel_2", "boot")
S3_MSE_independent <- calculate_MSE(data, "Study_3_Emanuel_2", "independent")
S3_MSE_parametric <- calculate_MSE(data, "Study_3_Emanuel_2", "parametric")
S3_MSE_cart <- calculate_MSE(data, "Study_3_Emanuel_2", "cart")

S4_MSE_boot <- calculate_MSE(data, "Study_4_Martin_2024", "boot")
S4_MSE_independent <- calculate_MSE(data, "Study_4_Martin_2024", "independent")
S4_MSE_parametric <- calculate_MSE(data, "Study_4_Martin_2024", "parametric")
S4_MSE_cart <- calculate_MSE(data, "Study_4_Martin_2024", "cart")

S5_MSE_boot <- calculate_MSE(data, "Study_5_St_Cyr_2024", "boot")
S5_MSE_independent <- calculate_MSE(data, "Study_5_St_Cyr_2024", "independent")
S5_MSE_parametric <- calculate_MSE(data, "Study_5_St_Cyr_2024", "parametric")
S5_MSE_cart <- calculate_MSE(data, "Study_5_St_Cyr_2024", "cart")

S6_MSE_boot <- calculate_MSE(data, "Study_6_Theobald_2022", "boot")
S6_MSE_independent <- calculate_MSE(data, "Study_6_Theobald_2022", "independent")
S6_MSE_parametric <- calculate_MSE(data, "Study_6_Theobald_2022", "parametric")
S6_MSE_cart <- calculate_MSE(data, "Study_6_Theobald_2022", "cart")

# Combining results into a data frame
results_for_plots <- data.frame(
  Study = factor(c("Study_1_Chen", "Study_1_Chen", "Study_1_Chen", "Study_1_Chen", 
                   "Study_2_Emanuel_1", "Study_2_Emanuel_1", "Study_2_Emanuel_1", "Study_2_Emanuel_1",
                   "Study_3_Emanuel_2", "Study_3_Emanuel_2", "Study_3_Emanuel_2", "Study_3_Emanuel_2",
                   "Study_4_Martin_2024", "Study_4_Martin_2024", "Study_4_Martin_2024", "Study_4_Martin_2024",
                   "Study_5_St_Cyr_2024", "Study_5_St_Cyr_2024", "Study_5_St_Cyr_2024", "Study_5_St_Cyr_2024",
                   "Study_6_Theobald_2022", "Study_6_Theobald_2022", "Study_6_Theobald_2022", "Study_6_Theobald_2022")),
  Method = c("boot", "independent", "parametric", "cart", 
             "boot", "independent", "parametric", "cart",
             "boot", "independent", "parametric", "cart",
             "boot", "independent", "parametric", "cart",
             "boot", "independent", "parametric", "cart",
             "boot", "independent", "parametric", "cart"),
  MSE = c(S1_MSE_boot, S1_MSE_independent, S1_MSE_parametric, S1_MSE_cart, 
          S2_MSE_boot, S2_MSE_independent, S2_MSE_parametric, S2_MSE_cart,
          S3_MSE_boot, S3_MSE_independent, S3_MSE_parametric, S3_MSE_cart,
          S4_MSE_boot, S4_MSE_independent, S4_MSE_parametric, S4_MSE_cart,
          S5_MSE_boot, S5_MSE_independent, S5_MSE_parametric, S5_MSE_cart,
          S6_MSE_boot, S6_MSE_independent, S6_MSE_parametric, S6_MSE_cart)
)



# Saving to a CSV file
write.csv(results_for_plots, "sport_psychology/Final_results/MSE_results.csv", row.names = FALSE)

# Renaming studies for better readability
results_for_plots$Study <- factor(results_for_plots$Study, 
                        levels = unique(results_for_plots$Study), 
                        labels = paste("Study", 1:6))

# Creating the plot with box plot and scatter plot with Study on x-axis
ggplot(results_for_plots, aes(x = Study, y = MSE, color = Method)) +
  geom_boxplot(aes(group = Study), alpha = 0.3) +  # Adding box plot
  geom_point(position = position_dodge(width = 0.5), size = 3) +  # Adding scatter plot
  scale_y_continuous(trans = 'log10', labels = scales::label_number(accuracy = 0.1)) +  # Scaling the y-axis and setting labels to non-scientific
  labs(title = "MSE Values by Study and Method",
       x = "Study",
       y = "MSE (log scale)") +
  theme_minimal()


## FACET
# # Creating the plot with box plot and scatter plot, scaling the y-axis, and adding enhancements
# ggplot(results_for_plots, aes(x = Study, y = MSE, color = Method)) +
#   geom_boxplot(aes(group = Study), alpha = 0.3) +  # Adding box plot
#   geom_point(size = 3) +  # Adding scatter plot without jitter
#   scale_y_continuous(trans = 'log10', labels = scales::label_number(accuracy = 0.01)) +  # Scaling the y-axis and setting labels to non-scientific with fewer decimal points
#   geom_text(aes(label = round(MSE, 2)), vjust = -0.5, size = 3) +  # Adding data labels
#   labs(title = "MSE Values by Study and Method",
#        x = "Study",
#        y = "MSE (log scale)",
#        color = "Method") +  # Adding legend title
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotating x-axis labels for better readability
#         panel.grid.major.x = element_blank(),  # Adding vertical grid lines
#         panel.grid.minor.x = element_blank()) +
#   facet_wrap(~ Method, scales = "free")  # Faceting by Method

# ggplot(results_for_plots, aes(x = Study, y = MSE, color = Method)) +
#   geom_boxplot(aes(group = Study), alpha = 0.3) +  # Adding box plot with correct alpha value
#   geom_point(size = 3) +  # Adding scatter plot without jitter
#   scale_y_continuous(trans = 'log10', labels = scales::label_number(accuracy = 0.01)) +  # Scaling the y-axis and setting labels to non-scientific with fewer decimal points
#   labs(title = "MSE Values by Study and Method",
#        x = "Study",
#        y = "MSE (log scale)") +
#   theme_minimal()
# 
# ggplot(results_for_plots, aes(x = Method, y = MSE, color = Study)) +
#   geom_boxplot(aes(group = Method), alpha = 0.3) +  # Adding box plot with correct alpha value
#   geom_point(size = 3) +  # Adding scatter plot without jitter
#   scale_y_continuous(trans = 'log10', labels = scales::label_number(accuracy = 0.01)) +  # Scaling the y-axis and setting labels to non-scientific with fewer decimal points
#   labs(title = "MSE Values by Study and Method",
#        x = "Study",
#        y = "MSE (log scale)") +
#   theme_minimal()

# no jittering, wide boxes
ggplot(results_for_plots, aes(x = Method, y = MSE, color = Study)) +
  geom_boxplot(aes(group = Method), alpha = 0.3, width = 0.5) +  # Adding box plot with correct alpha value and narrower width
  geom_point(size = 3, position = position_dodge(width = 0.5)) +  # Adding scatter plot without jitter and aligning with boxplots
  scale_y_continuous(trans = 'log10', labels = scales::label_number(accuracy = 0.01)) +  # Scaling the y-axis and setting labels to non-scientific with fewer decimal points
  labs(title = "MSE Values by Study and Method",
       x = "Method",
       y = "MSE (log scale)") +
  theme_minimal()

# with jittering, narrow boxes
ggplot(results_for_plots, aes(x = Method, y = MSE, color = Study)) +
  geom_boxplot(aes(group = Method), alpha = 0.3, width = 0.4, outlier.shape = NA) +  # Adding box plot with correct alpha value and narrower width
  geom_point(size = 3, position = position_dodge(width = 0.5)) +  # Adding scatter plot with points always in the same order
  scale_y_continuous(trans = 'log10', labels = scales::label_number(accuracy = 0.01)) +  # Scaling the y-axis and setting labels to non-scientific with fewer decimal points
  labs(title = "MSE Values by Study and Method",
       x = "Method",
       y = "MSE (log scale)") +
  theme_minimal()





# mean_CI_overlap <- data |>
#   filter(STUDY == "Study_1_Chen", METHOD == "boot") |>
#   summarise(avg_CI_OVERLAP = mean(CI_OVERLAP))
# 
# print(mean_CI_overlap)



## CONFIDENCE INTERVAL OVERLAP
# 
# # Define the function for CIO
# mean_CI_overlap <- function(data, study_name, method_name) {
#   result <- data |>
#     filter(STUDY == study_name, METHOD == method_name) |>
#     summarise(avg_CI_OVERLAP = mean(CI_OVERLAP, na.rm = TRUE))
#   
#   return(result)
# }

mean_CI_overlap <- function(data, study_name, method_name) {
  result <- data |>
    filter(STUDY == study_name, METHOD == method_name) |>
    summarise(avg_CI_OVERLAP = mean(CI_OVERLAP, na.rm = TRUE)) |>
    pull(avg_CI_OVERLAP)
  
  return(result)
}



# Calculating CIO values for Study 1
S1_CIO_boot <- mean_CI_overlap(data, "Study_1_Chen", "boot")
S1_CIO_independent <- mean_CI_overlap(data, "Study_1_Chen", "independent")
S1_CIO_parametric <- mean_CI_overlap(data, "Study_1_Chen", "parametric")
S1_CIO_cart <- mean_CI_overlap(data, "Study_1_Chen", "cart")

# Calculating CIO values for Study 2
S2_CIO_boot <- mean_CI_overlap(data, "Study_2_Emanuel_1", "boot")
S2_CIO_independent <- mean_CI_overlap(data, "Study_2_Emanuel_1", "independent")
S2_CIO_parametric <- mean_CI_overlap(data, "Study_2_Emanuel_1", "parametric")
S2_CIO_cart <- mean_CI_overlap(data, "Study_2_Emanuel_1", "cart")

# Calculating CIO values for Study 3
S3_CIO_boot <- mean_CI_overlap(data, "Study_3_Emanuel_2", "boot")
S3_CIO_independent <- mean_CI_overlap(data, "Study_3_Emanuel_2", "independent")
S3_CIO_parametric <- mean_CI_overlap(data, "Study_3_Emanuel_2", "parametric")
S3_CIO_cart <- mean_CI_overlap(data, "Study_3_Emanuel_2", "cart")

# Calculating CIO values for Study 4
S4_CIO_boot <- mean_CI_overlap(data, "Study_4_Martin_2024", "boot")
S4_CIO_independent <- mean_CI_overlap(data, "Study_4_Martin_2024", "independent")
S4_CIO_parametric <- mean_CI_overlap(data, "Study_4_Martin_2024", "parametric")
S4_CIO_cart <- mean_CI_overlap(data, "Study_4_Martin_2024", "cart")

# Calculating CIO values for Study 5
S5_CIO_boot <- mean_CI_overlap(data, "Study_5_St_Cyr_2024", "boot")
S5_CIO_independent <- mean_CI_overlap(data, "Study_5_St_Cyr_2024", "independent")
S5_CIO_parametric <- mean_CI_overlap(data, "Study_5_St_Cyr_2024", "parametric")
S5_CIO_cart <- mean_CI_overlap(data, "Study_5_St_Cyr_2024", "cart")

# Calculating CIO values for Study 6
S6_CIO_boot <- mean_CI_overlap(data, "Study_6_Theobald_2022", "boot")
S6_CIO_independent <- mean_CI_overlap(data, "Study_6_Theobald_2022", "independent")
S6_CIO_parametric <- mean_CI_overlap(data, "Study_6_Theobald_2022", "parametric")
S6_CIO_cart <- mean_CI_overlap(data, "Study_6_Theobald_2022", "cart")


# Combining CIO results into a data frame
cio_results <- data.frame(
  Study = factor(c("Study_1_Chen", "Study_1_Chen", "Study_1_Chen", "Study_1_Chen", 
                   "Study_2_Emanuel_1", "Study_2_Emanuel_1", "Study_2_Emanuel_1", "Study_2_Emanuel_1",
                   "Study_3_Emanuel_2", "Study_3_Emanuel_2", "Study_3_Emanuel_2", "Study_3_Emanuel_2",
                   "Study_4_Martin_2024", "Study_4_Martin_2024", "Study_4_Martin_2024", "Study_4_Martin_2024",
                   "Study_5_St_Cyr_2024", "Study_5_St_Cyr_2024", "Study_5_St_Cyr_2024", "Study_5_St_Cyr_2024",
                   "Study_6_Theobald_2022", "Study_6_Theobald_2022", "Study_6_Theobald_2022", "Study_6_Theobald_2022")),
  Method = c("boot", "independent", "parametric", "cart", 
             "boot", "independent", "parametric", "cart",
             "boot", "independent", "parametric", "cart",
             "boot", "independent", "parametric", "cart",
             "boot", "independent", "parametric", "cart",
             "boot", "independent", "parametric", "cart"),
  CIO = c(S1_CIO_boot, S1_CIO_independent, S1_CIO_parametric, S1_CIO_cart, 
          S2_CIO_boot, S2_CIO_independent, S2_CIO_parametric, S2_CIO_cart,
          S3_CIO_boot, S3_CIO_independent, S3_CIO_parametric, S3_CIO_cart,
          S4_CIO_boot, S4_CIO_independent, S4_CIO_parametric, S4_CIO_cart,
          S5_CIO_boot, S5_CIO_independent, S5_CIO_parametric, S5_CIO_cart,
          S6_CIO_boot, S6_CIO_independent, S6_CIO_parametric, S6_CIO_cart)
)

# Saving CIO results to a CSV file
write.csv(cio_results, "sport_psychology/Final_results/CIO_means.csv", row.names = FALSE)



# Renaming studies for better readability
cio_results$Study <- factor(cio_results$Study, 
                            levels = unique(cio_results$Study), 
                            labels = paste("Study", 1:6))

# Creating the plot with box plot and scatter plot, and scaling the y-axis
library(ggplot2)

ggplot(cio_results, aes(x = Method, y = CIO, color = Study)) +
  geom_boxplot(aes(group = Method), width = 0.5, alpha = 0.3, outlier.shape = NA) +  # Adding box plot
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.5), size = 3) +  # Adding jittered points
  labs(title = "CI Overlap Values by Study and Method",
       x = "Method",
       y = "CI Overlap (log scale)") +
  theme_minimal() 




# Saving the plot to a file
#ggsave("CIO_plot_with_boxplot.png", width = 10, height = 6)


####


#testing


# Assuming your data is in a data frame called df
# Filter out "original" values from METHOD
filtered_df <- data %>% filter(METHOD != "original" & !is.na(CI_OVERLAP))


# Create the ggplot with improved readability
ggplot(filtered_df, aes(x = METHOD, y = CI_OVERLAP, color = STUDY)) +
  geom_point(size = 1, alpha = 0.7, position = position_jitter(width = 0.2, height = 0)) +
  scale_color_brewer(palette = "Set2") +
  theme_minimal(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  labs(x = "Method", y = "CI Overlap", color = "Study")

# Create the ggplot with jittering and a narrow boxplot
ggplot(filtered_df, aes(x = METHOD, y = CI_OVERLAP, color = STUDY)) +
  geom_jitter(size = 1, alpha = 0.7, position = position_jitter(width = 0.2, height = 0)) +
  geom_boxplot(aes(group = METHOD), width = 0.2, alpha = 0.5, outlier.shape = NA, color = "black") +
  scale_color_brewer(palette = "Set2") +
  theme_minimal(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  labs(x = "Method", y = "CI Overlap", color = "Study")


# Create the ggplot with enhanced jittering and a single boxplot per method
ggplot(filtered_df, aes(x = METHOD, y = CI_OVERLAP, color = STUDY)) +
  geom_jitter(size = 1, alpha = 0.7, width = 0.3, height = 0.1) +
  geom_boxplot(aes(group = METHOD), width = 0.2, alpha = 0.5, outlier.shape = NA, color = "black") +
  scale_color_brewer(palette = "Set2") +
  theme_minimal(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  labs(x = "Method", y = "CI Overlap", color = "Study")



# Create the ggplot with enhanced jittering and a single boxplot per method
ggplot(filtered_df, aes(x = METHOD, y = CI_OVERLAP, color = STUDY)) +
  geom_jitter(size = 1, alpha = 0.7, width = 0.3, height = 0) +
  geom_boxplot(aes(group = METHOD), width = 0.2, alpha = 0.5, outlier.shape = NA, color = "black") +
  scale_color_brewer(palette = "Set2") +
  theme_minimal(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  labs(x = "Method", y = "CI Overlap", color = "Study") 


library(ggplot2)

ggplot(filtered_df, aes(x = METHOD, y = CI_OVERLAP, color = STUDY)) +
  geom_jitter(size = 0.5, alpha = 0.5, width = 0.3, height = 0) +
  geom_boxplot(aes(group = METHOD), width = 0.2, alpha = 0.5, outlier.shape = NA, color = "black") +
  geom_violin(aes(group = METHOD), alpha = 0.2, fill = "grey80") +
  scale_color_brewer(palette = "Set2") +
  theme_minimal(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  labs(x = "Method", y = "CI Overlap", color = "Study")

ggplot(filtered_df, aes(x = METHOD, y = CI_OVERLAP, color = STUDY)) +
  geom_jitter(size = 0.5, alpha = 0.5, width = 0.3, height = 0) +
  geom_boxplot(aes(group = METHOD), width = 0.2, alpha = 0.5, outlier.shape = NA, color = "black") +
  scale_color_brewer(palette = "Set2") +
  theme_minimal(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  labs(x = "Method", y = "CI Overlap", color = "Study") +
  facet_wrap(~ STUDY, scales = "free_y")


library(ggplot2)
library(ggplot2)

ggplot(filtered_df, aes(x = METHOD, y = CI_OVERLAP, color = STUDY)) +
  geom_violin(aes(fill = METHOD), alpha = 0.3, color = NA) +
  geom_jitter(size = 0.5, alpha = 0.5, width = 0.3, height = 0) +
  geom_boxplot(aes(group = METHOD), width = 0.2, alpha = 0.5, outlier.shape = NA, color = "black") +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  labs(x = "Method", y = "CI Overlap", color = "Study", fill = "Method")




library(ggplot2)

ggplot(filtered_df, aes(x = METHOD, y = CI_OVERLAP, color = STUDY)) +
  geom_violin(aes(fill = METHOD), alpha = 0.3, color = NA) +
  geom_jitter(size = 0.5, alpha = 0.5, width = 0.3, height = 0) +
  geom_boxplot(aes(group = METHOD), width = 0.2, alpha = 0.5, outlier.shape = NA, color = "black") +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  coord_cartesian(ylim = c(-1.25, 1)) +
  theme_minimal(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  labs(x = "Method", y = "CI Overlap", color = "Study", fill = "Method")








# Load necessary libraries
library(ggplot2)
library(dplyr)

# Assuming your data is in a data frame called df
# Filter out "original" values from METHOD and remove NA values in CI_OVERLAP
filtered_df <- data %>% filter(METHOD != "original" & !is.na(CI_OVERLAP))

# Sample 5 points per study and method combination
set.seed(123) # for reproducibility
sampled_df <- filtered_df %>%
  group_by(STUDY, METHOD) %>%
  sample_n(5, replace = TRUE) %>% # Use replace = TRUE to ensure 5 samples if fewer points available
  ungroup()

# Calculate density around each point
calculate_density <- function(df, radius = 0.1) {
  df$density <- sapply(1:nrow(df), function(i) {
    sum(sqrt((df$CI_OVERLAP - df$CI_OVERLAP[i])^2) <= radius)
  })
  return(df)
}

sampled_df <- calculate_density(sampled_df)

# Create the ggplot with jittering
ggplot(sampled_df, aes(x = METHOD, y = CI_OVERLAP, color = STUDY, size = density)) +
  geom_jitter(alpha = 0.7, width = 0.3, height = 0) +
  geom_boxplot(aes(group = METHOD), width = 0.2, alpha = 0.2, outlier.shape = NA, color = "black") +
  scale_color_brewer(palette = "Set2") +
  scale_size_continuous(range = c(1, 6)) +
  theme_minimal(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  labs(x = "Method", y = "CI Overlap", color = "Study", size = "Density")





###### CIO ALL ESTIMATES


# Create the new dataframe
CIO_all_estimates <- data |>
  dplyr::filter(METHOD != 'original') |>
  dplyr::select(STUDY, METHOD, CI_OVERLAP)  # Select the required columns

write.csv(CIO_all_estimates, "sport_psychology/Final_results/CIO_all_estimates.csv", row.names = FALSE)







ggplot(CIO_all_estimates, aes(x = METHOD, y = CI_OVERLAP, color = STUDY)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), alpha = 0.6) +
  theme_minimal() +
  labs(title = "CI Overlap by Study and Method",
       x = "Study",
       y = "CI Overlap",
       color = "Method") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## FACET
ggplot(CIO_all_estimates, aes(x = METHOD, y = CI_OVERLAP, color = STUDY)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), alpha = 0.6, size = 2) +
  theme_minimal() +
  labs(title = "CI Overlap by Study and Method",
       x = "Method",
       y = "CI Overlap",
       color = "Study") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(~ STUDY, scales = "free_y")

# Create the plot with box plots and facets
ggplot(CIO_all_estimates, aes(x = METHOD, y = CI_OVERLAP, color = STUDY)) +
  geom_boxplot(alpha = 0.6) +
  geom_point(position = position_jitter(width = 0.2, height = 0), alpha = 0.6, size = 2) +
  theme_minimal() +
  labs(title = "CI Overlap by Study and Method",
       x = "Method",
       y = "CI Overlap",
       color = "Study") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  facet_wrap(~ STUDY, scales = "free_y", nrow = 3)

ggplot(CIO_all_estimates, aes(x = METHOD, y = CI_OVERLAP)) +
  geom_boxplot(width = 0.5, alpha = 0.6) +
  geom_point(aes(color = STUDY), position = position_jitter(width = 0.2, height = 0), alpha = 0.6, size = 2) +
  theme_minimal() +
  labs(title = "CI Overlap by Study and Method",
       x = "Method",
       y = "CI Overlap",
       color = "Study") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  facet_wrap(~ STUDY, scales = "free_y", nrow = 3)



# Get unique study names
studies <- unique(CIO_all_estimates$STUDY)

# Create individual plots for each study
for (study in studies) {
  plot <- ggplot(CIO_all_estimates %>% filter(STUDY == study), aes(x = METHOD, y = CI_OVERLAP)) +
    geom_boxplot(alpha = 0.6) +
    geom_point(aes(color = STUDY), position = position_jitter(width = 0.2, height = 0), alpha = 0.6, size = 2) +
    theme_minimal() +
    labs(title = paste("CI Overlap for", study, "by Method"),
         x = "Method",
         y = "CI Overlap",
         color = "Study") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
  print(plot)
}




ggplot(CIO_all_estimates, aes(x = METHOD, y = CI_OVERLAP)) +
  geom_violin(aes(fill = METHOD), alpha = 0.6, trim = FALSE) +
  geom_jitter(aes(color = STUDY), position = position_jitter(width = 0.2, height = 0), alpha = 0.4, size = 1) +
  theme_minimal() +
  labs(title = "CI Overlap by Method",
       x = "Method",
       y = "CI Overlap",
       fill = "Method",
       color = "Study") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


# Create the box plot
ggplot(CIO_all_estimates, aes(x = METHOD, y = CI_OVERLAP)) +
  geom_boxplot(aes(fill = METHOD), alpha = 0.6) +
  geom_jitter(aes(color = STUDY), position = position_jitter(width = 0.2, height = 0), alpha = 0.4, size = 1) +
  theme_minimal() +
  labs(title = "CI Overlap by Method",
       x = "Method",
       y = "CI Overlap",
       fill = "Method",
       color = "Study") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))





Absolute_error <- data |>
  dplyr::filter(METHOD != 'original') |>
  dplyr::mutate(DIFFENCE_ORIGINAL = abs(DIFFENCE_ORIGINAL)) |>
  dplyr::select(STUDY, METHOD, ESTIMATE_NAME, DIFFENCE_ORIGINAL)  # Select the required columns





## ABSOLUTE ERROR OF ESTIMATES
ggplot(Absolute_error, aes(x = METHOD, y = DIFFENCE_ORIGINAL)) +
  geom_boxplot(width = 0.5, alpha = 0.6) +
  geom_point(aes(color = STUDY), position = position_jitter(width = 0.2, height = 0), alpha = 0.6, size = 2) +
  theme_minimal() +
  labs(title = "Absolute Error by Study and Method",
       x = "Method",
       y = "Absolute Error",
       color = "Study") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  facet_wrap(~ STUDY, scales = "free_y", nrow = 3)



### MSE 

# with jittering, narrow boxes
ggplot(results_for_plots, aes(x = Method, y = MSE, color = Study)) +
  geom_boxplot(aes(group = Method), alpha = 0.3, width = 0.4, outlier.shape = NA) +  # Adding box plot with correct alpha value and narrower width
  geom_point(size = 3, position = position_dodge(width = 0.5)) +  # Adding scatter plot with points always in the same order
  scale_y_continuous(trans = 'log10', labels = scales::label_number(accuracy = 0.01)) +  # Scaling the y-axis and setting labels to non-scientific with fewer decimal points
  labs(title = "MSE Values by Study and Method",
       x = "Method",
       y = "MSE (log scale)") +
  theme_minimal()



### CIO VALUES OF ALL ESTIMATES


ggplot(CIO_all_estimates, aes(x = METHOD, y = CI_OVERLAP, fill = STUDY)) +
  geom_violin(alpha = 0.6, trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75)) +
  #stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "black") +
  theme_minimal() +
  labs(title = "CI Overlap by Study and Method",
       x = "Method",
       y = "CI Overlap",
       fill = "Study") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


library(ggplot2)
library(gridExtra)

# Create a base plot function to reuse for each method
plot_violin <- function(data, method_name) {
  ggplot(data[data$METHOD == method_name, ], aes(x = STUDY, y = CI_OVERLAP, fill = STUDY)) +
    geom_violin(alpha = 0.6, trim = TRUE, draw_quantiles = c(0.25, 0.5, 0.75)) +
    theme_minimal() +
    labs(title = paste("CI Overlap by Study for", method_name),
         x = "Study",
         y = "CI Overlap",
         fill = "Study") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}

# List of unique methods
methods <- unique(CIO_all_estimates$METHOD)

# Generate plots for each method
plots <- lapply(methods, function(method) plot_violin(CIO_all_estimates, method))

# Arrange plots in a 2x2 grid
grid.arrange(grobs = plots, ncol = 2)






ggplot(CIO_all_estimates, aes(x = METHOD, y = CI_OVERLAP, fill = STUDY)) +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.7) +
  theme_minimal() +
  labs(title = "CI Overlap by Study and Method",
       x = "Method",
       y = "CI Overlap",
       fill = "Study") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  facet_wrap(~ STUDY, scales = "free_y", nrow = 3)

library(ggplot2)

ggplot(CIO_all_estimates, aes(x = METHOD, y = CI_OVERLAP, fill = STUDY)) +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5, binwidth = 0.1, position = position_dodge(width = 0.75)) +
  geom_boxplot(alpha = 0.5, position = position_dodge(width = 0.75), outlier.shape = NA) +
  theme_minimal() +
  labs(title = "CI Overlap by Study and Method",
       x = "Method",
       y = "CI Overlap",
       fill = "Study") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  facet_wrap(~ STUDY, scales = "free_y", nrow = 3)


library(ggplot2)

ggplot(CIO_all_estimates, aes(x = METHOD, y = CI_OVERLAP, fill = STUDY)) +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5, binwidth = 0.1, position = position_dodge(width = 0.75)) +
  #geom_boxplot(alpha = 0.5, position = position_dodge(width = 0.75), outlier.shape = NA) +
  theme_minimal() +
  labs(title = "CI Overlap by Study and Method",
       x = "Method",
       y = "CI Overlap",
       fill = "Study") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


library(ggplot2)

ggplot(CIO_all_estimates, aes(x = METHOD, y = CI_OVERLAP, fill = STUDY)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75), size = 0.5, alpha = 0.6) +
  geom_boxplot(alpha = 0.5, position = position_dodge(width = 0.75), outlier.shape = NA) +
  theme_minimal() +
  labs(title = "CI Overlap by Study and Method",
       x = "Method",
       y = "CI Overlap",
       fill = "Study") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


library(ggplot2)

ggplot(CIO_all_estimates, aes(x = STUDY, y = CI_OVERLAP, color = STUDY, fill = STUDY)) +
  geom_violin(alpha = 0.5, position = position_dodge(width = 0.75), draw_quantiles = c(0.25, 0.5, 0.75), width = 1) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', stackratio = 0.5, dotsize = 0.5, binwidth = 0.05, position = position_dodge(width = 0.75)) +
  facet_wrap(~ METHOD, scales = "fixed")+
  theme_minimal() +
  labs(title = "CI Overlap by Study and Method",
       x = "Method",
       y = "CI Overlap",
       fill = "Study",
       color = "Study") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  


library(ggplot2)

ggplot(CIO_all_estimates, aes(x = STUDY, y = CI_OVERLAP, color = STUDY, fill = STUDY)) +
  geom_boxplot(alpha = 0.5, position = position_dodge(width = 0.75), outlier.shape = NA) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', stackratio = 1.5, dotsize = 0.5, binwidth = 0.05, position = position_dodge(width = 0.75)) +
  facet_wrap(~ METHOD, scales = "fixed") +
  theme_minimal() +
  labs(title = "CI Overlap by Study and Method",
       x = "Study",
       y = "CI Overlap",
       fill = "Study",
       color = "Study") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))



library(ggplot2)

ggplot(CIO_all_estimates, aes(x = METHOD, y = CI_OVERLAP, color = STUDY)) +
  geom_point( position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75), size = 1, alpha = 0.7) +
  geom_boxplot(alpha = 0.5, position = position_dodge(width = 0.75), outlier.shape = NA) +
  theme_minimal() +
  labs(title = "CI Overlap by Study and Method",
       x = "Method",
       y = "CI Overlap",
       color = "Study") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))





ggplot(CIO_all_estimates, aes(x = METHOD, y = CI_OVERLAP)) +
  geom_boxplot(width = 0.5, alpha = 0.6) +
  geom_point(aes(color = STUDY), position = position_jitter(width = 0.2, height = 0), alpha = 0.6, size = 2) +
  theme_minimal() +
  labs(title = "CI Overlap by Study and Method",
       x = "Method",
       y = "CI Overlap",
       color = "Study") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  facet_wrap(~ STUDY, scales = "free_y", nrow = 3)

library(ggplot2)

ggplot(CIO_all_estimates, aes(x = METHOD, y = CI_OVERLAP)) +
  geom_boxplot(width = 0.5, alpha = 0.6) +
  geom_point(aes(color = STUDY), position = position_jitter(width = 0.2, height = 0), alpha = 0.6, size = 2) +
  theme_minimal() +
  labs(title = "CI Overlap by Study and Method",
       x = "Method",
       y = "CI Overlap",
       color = "Study") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  facet_wrap(~ STUDY, scales = "fixed", nrow = 3)



# Get unique study names
studies <- unique(CIO_all_estimates$STUDY)

# Create individual plots for each study
for (study in studies) {
  plot <- ggplot(CIO_all_estimates %>% filter(STUDY == study), aes(x = METHOD, y = CI_OVERLAP)) +
    geom_boxplot(alpha = 0.6) +
    geom_point(aes(color = "deeppink"), position = position_jitter(width = 0.2, height = 0), alpha = 0.6, size = 2) +
    theme_minimal() +
    labs(title = paste("CI Overlap for", study, "by Method"),
         x = "Method",
         y = "CI Overlap",
         color = "deeppink") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
  print(plot)
}



### CIO MEAN VALUES

ggplot(cio_results, aes(x = Method, y = CIO, color = Study)) +
  geom_boxplot(aes(group = Method), width = 0.5, alpha = 0.3, outlier.shape = NA) +  # Adding box plot
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.5), size = 3) +  # Adding jittered points
  labs(title = "CI Overlap Values by Study and Method",
       x = "Method",
       y = "CI Overlap (log scale)") +
  scale_y_continuous(limits = c(-0.6, 1)) +  # Setting the y-axis limits
  theme_minimal() 




### FOR POSTER

# Adjusting the ggplot code to fit a scientific poster style with the given color palette
library(ggplot2)
library(scales)

# Assuming `results_for_plots` is your dataframe
ggplot(results_for_plots, aes(x = Method, y = MSE, color = Study)) +
  geom_boxplot(aes(group = Method), alpha = 0.3, width = 0.4, outlier.shape = NA) +  # Adding box plot with correct alpha value and narrower width
  geom_point(size = 10, position = position_dodge(width = 0.0)) +  # Adding scatter plot with points always in the same order
  scale_y_continuous(trans = 'log10', labels = label_number(accuracy = 0.01)) +  # Scaling the y-axis and setting labels to non-scientific with fewer decimal points
  labs(title = "MSE Values by Study and Method",
       x = "Method",
       y = "MSE (log scale)") +
  theme_minimal() +
  scale_color_manual(values = c("#0A210F", "#14591D", "#99AA38", "#E1E289", "#ACD2ED", "#AFD29D")) +  # Applying the given color palette
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    panel.background = element_rect(fill = "#99AA38", color = "#99AA38"),  # Setting the background color
    plot.background = element_rect(fill = "#99AA38", color = "#99AA38")  # Setting the background color
  )


# Adjusting the ggplot code to fit a scientific poster style with the given color palette, background color, no grid, no axes, and MSE values inside each dot
library(ggplot2)
library(scales)

# Assuming `results_for_plots` is your dataframe
p <- ggplot(results_for_plots, aes(x = Method, y = MSE, color = Study, label = sprintf("%.2f", MSE))) +
  geom_boxplot(aes(group = Method), alpha = 0.6, width = 0.6, outlier.shape = NA) +  # Adding box plot with correct alpha value and narrower width
  geom_point(size = 15, position = position_dodge(width = 0.6)) +  # Adding scatter plot with points always in the same order
  #geom_text(position = position_dodge(width = 0.5), vjust = -1.5, size = 5, color = "white", size = 5, fontface = "bold") +  # Adding MSE values inside each dot
  scale_y_continuous(trans = 'log10', labels = label_number(accuracy = 0.01)) +  # Scaling the y-axis and setting labels to non-scientific with fewer decimal points
  labs(title = "",
       x = "Method",
       y = "MSE (log scale)") +
  theme_minimal() +
  scale_color_manual(values = c("#0A210F", "#14591D", "#99FF38", "#E1E289", "#ACD2ED", "#AFD29D")) +  # Applying the given color palette
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    panel.background = element_rect(fill = "transparent", color = NA),  # Setting the background color
    plot.background = element_rect(fill = "transparent", color = NA),  # Setting the background color
    panel.grid.major = element_blank(),  # Removing major grid lines
    panel.grid.minor = element_blank(),  # Removing minor grid lines
    axis.ticks = element_blank(),  # Removing axis ticks
    axis.title.x = element_blank(),  # Removing x-axis title
    axis.title.y = element_blank(),  # Removing y-axis title
    axis.text.y = element_blank()  # Removing y-axis text
  )

# Save the plot with a transparent background
ggsave("transparent_plot.png", plot = p, bg = "transparent")



# Adjusting the ggplot code to fit a scientific poster style with the given color palette, background color, no grid, no axes, and MSE values inside each dot
library(ggplot2)
library(scales)

# Assuming `results_for_plots` is your dataframe
ggplot(results_for_plots, aes(x = Method, y = MSE, color = Study, label = sprintf("%.2f", MSE))) +
  geom_boxplot(aes(group = Method), alpha = 0.3, width = 0.4, outlier.shape = NA) +  # Adding box plot with correct alpha value and narrower width
  geom_point(size = 3, position = position_jitter(width = 0.5, height = 0)) +  # Jittering points for better visibility
  geom_text(position = position_jitter(width = 0.5, height = 0), color = "white", size = 5, fontface = "bold", vjust = 0.5, hjust = 0.5) +  # Adding centered MSE values inside each dot with specified style
  scale_y_continuous(trans = 'log10', labels = label_number(accuracy = 0.01)) +  # Scaling the y-axis and setting labels to non-scientific with fewer decimal points
  labs(title = "MSE Values by Study and Method",
       x = "Method",
       y = "MSE (log scale)") +
  theme_minimal() +
  scale_color_manual(values = c("#0A210F", "#14591D", "#99AA38", "#E1E289", "#ACD2ED", "white")) +  # Applying the given color palette
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    panel.background = element_rect(fill = "#99AA38", color = "#99AA38"),  # Setting the background color
    plot.background = element_rect(fill = "#99AA38", color = "#99AA38"),  # Setting the background color
    panel.grid.major = element_blank(),  # Removing major grid lines
    panel.grid.minor = element_blank(),  # Removing minor grid lines
    axis.ticks = element_blank(),  # Removing axis ticks
    axis.title.x = element_blank(),  # Removing x-axis title
    axis.title.y = element_blank(),  # Removing y-axis title
    axis.text.x = element_blank(),  # Removing x-axis text
    axis.text.y = element_blank()  # Removing y-axis text
  )




# Assuming your data frame is named df
library(dplyr)

# Group by Study and Method and summarize to get min and max CI_OVERLAP
df_summary <- CIO_all_estimates %>%
  group_by(STUDY, METHOD) %>%
  summarise(
    MIN_CI_OVERLAP = min(CI_OVERLAP, na.rm = TRUE),
    MAX_CI_OVERLAP = max(CI_OVERLAP, na.rm = TRUE),
    MEAN_CI_OVERLAP = mean(CI_OVERLAP, na.rm = TRUE)
  )

# View the summary data frame
print(df_summary)

write.csv(df_summary, "sport_psychology/Final_results/CIO_ranges.csv", row.names = FALSE)

