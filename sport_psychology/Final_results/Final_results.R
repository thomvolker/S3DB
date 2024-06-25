library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)

# read in result file
data <- read_excel("sport_psychology/Final_results/final_results.xlsx")


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


### RMSE KEEP

# with jittering, narrow boxes
ggplot(results_for_plots, aes(x = Method, y = MSE, color = Study)) +
  geom_boxplot(aes(group = Method), alpha = 0.3, width = 0.4, outlier.shape = NA) +  # Adding box plot with correct alpha value and narrower width
  geom_point(size = 3, position = position_dodge(width = 0.5)) +  # Adding scatter plot with points always in the same order
  scale_y_continuous(trans = 'log10', labels = scales::label_number(accuracy = 0.01)) +  # Scaling the y-axis and setting labels to non-scientific with fewer decimal points
  labs(title = "MSE Values by Study and Method",
       x = "Method",
       y = "MSE (log scale)") +
  theme_minimal()



# Function to calculate mean CIO values
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


#### MEAN CIO VALUES?
library(ggplot2)

ggplot(cio_results, aes(x = Method, y = CIO, color = Study)) +
  geom_boxplot(aes(group = Method), width = 0.5, alpha = 0.3, outlier.shape = NA) +  # Adding box plot
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.5), size = 3) +  # Adding jittered points
  labs(title = "CI Overlap Values by Study and Method",
       x = "Method",
       y = "CI Overlap (log scale)") +
  theme_minimal() 



# 
# # Assuming your data is in a data frame called df
# # Filter out "original" values from METHOD
# filtered_df <- data %>% filter(METHOD != "original" & !is.na(CI_OVERLAP))






###### CIO ALL ESTIMATES


# Create the new dataframe
CIO_all_estimates <- data |>
  dplyr::filter(METHOD != 'original') |>
  dplyr::select(STUDY, METHOD, CI_OVERLAP)  # Select the required columns

write.csv(CIO_all_estimates, "sport_psychology/Final_results/CIO_all_estimates.csv", row.names = FALSE)


### CIO ALL VALUES FACET KEEP

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
    geom_point(aes(), position = position_jitter(width = 0.2, height = 0), alpha = 0.6, size = 2) +
    theme_minimal() +
    labs(title = paste("Confidence Interval Overlap for", study, "by Method"),
         x = "Method",
         y = "CIO",
         color = "Study") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

  print(plot)
}





#### ABSOLUTE ERROR PLOTS KEEP
Absolute_error <- data |>
  dplyr::filter(METHOD != 'original') |>
  dplyr::mutate(DIFFENCE_ORIGINAL = abs(DIFFENCE_ORIGINAL)) |>
  dplyr::select(STUDY, METHOD, ESTIMATE_NAME, DIFFENCE_ORIGINAL)  # Select the required columns


# Rename studies
Absolute_error <- Absolute_error %>%
  mutate(STUDY = factor(STUDY, levels = unique(STUDY), labels = paste("Study", 1:length(unique(STUDY)))))


Absolute_error <- Absolute_error %>%
  mutate(METHOD = case_when(
    METHOD == "boot" ~ "Bootstrap",
    METHOD == "cart" ~ "CART",
    METHOD == "independent" ~ "Independent",
    METHOD == "parametric" ~ "Parametric",
    TRUE ~ METHOD  # Keep original value if no match
  ))


ggplot(Absolute_error, aes(x = METHOD, y = DIFFENCE_ORIGINAL)) +
  geom_boxplot(width = 0.5, alpha = 0.6) +
  geom_point(aes(color = METHOD), position = position_jitter(width = 0.2, height = 0), alpha = 0.6, size = 2) +
  theme_minimal() +
  labs(title = "Absolute Error by Study and Method",
       x = "Method",
       y = "Absolute Error",
       color = "Method") +
  facet_wrap(~ STUDY, scales = "free_y", nrow = 3)



### MSE FOR POSTER

# 
# # Assuming you have a named vector for renaming the methods
# method_names <- c("boot" = "Bootstrap", "cart" = "CART", "independent" = "Independent", "parametric" = "Parametric")
# 
# colors <- c("#68A691", "#7D82B8", "#F49D37", "#D81159", "#8F2D56", "#0D3B66")
# 
# mse <- ggplot(results_for_plots, aes(x = Method, y = MSE, color = Study)) +
#   geom_boxplot(aes(group = Method), alpha = 0.3, width = 0.4, outlier.shape = NA) +  # Adding box plot with correct alpha value and narrower width
#   geom_point(size = 5, position = position_dodge(width = 0.5)) +  # Making the dots bigger
#   scale_y_continuous(trans = 'log10', labels = scales::label_number(accuracy = 0.01)) +  # Scaling the y-axis and setting labels to non-scientific with fewer decimal points
#   scale_x_discrete(labels = method_names) +  # Renaming methods on x-axis
#   scale_color_manual(values = colors) +  
#   labs(title = "MSE Values by Study and Method",
#        x = "Synthetic Data Generation Method",
#        y = "RMSE (log scale)") +
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = 0.5))  # Centering the title
# 
# mse
# # Save the plot
# ggsave("mse_plot.png", plot = mse, width = 8, height = 4)
# 
# 
# 
# 
# 
# library(ggplot2)
# library(extrafont)  # Needed to use custom fonts
# 
# # Load fonts (only necessary once per session)
# loadfonts(device = "mac")  # or use "mac" or "pdf" depending on your platform
# 
# # Assuming you have a named vector for renaming the methods
# method_names <- c("boot" = "BOOTSTRAP", "cart" = "CART", "independent" = "INDEPENDENT", "parametric" = "PARAMETRIC")
# 
# colors <- c("#68A691", "#7D82B8", "#F49D37", "#D81159", "#8F2D56", "#0D3B66")
# 
# mse <- ggplot(results_for_plots, aes(x = Method, y = MSE, color = Study)) +
#   geom_boxplot(aes(group = Method), alpha = 0.3, width = 0.4, outlier.shape = NA) +  # Adding box plot with correct alpha value and narrower width
#   geom_point(size = 5, position = position_dodge(width = 0.5)) +  # Making the dots bigger
#   scale_y_continuous(trans = 'log10', labels = scales::label_number(accuracy = 0.01)) +  # Scaling the y-axis and setting labels to non-scientific with fewer decimal points
#   scale_x_discrete(labels = method_names) +  # Renaming methods on x-axis
#   scale_color_manual(values = colors) +  
#   labs(title = "RMSE Values by Study and Method",
#        x = "Synthetic Data Generation Method",
#        y = "RMSE (log scale)") +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5, family = "Aptos", color = "black"),  # Centering and setting font and color of the title
#     axis.title = element_text(family = "Aptos", color = "black"),  # Setting font and color of axis titles
#     axis.text = element_text(family = "Aptos", color = "black")  # Setting font and color of axis text
#   )
# 
# mse
# 
# # Save the plot
# ggsave("mse_plot.png", plot = mse, width = 8, height = 4)
# 




### CIO VALUES OF ALL ESTIMATES KEEP
library(ggplot2)
library(gridExtra)
library(ggpubr)  # For arranging ggplots with a common legend
library(dplyr)   # For data manipulation

CIO_all_estimates <- CIO_all_estimates %>%
  mutate(METHOD = case_when(
    METHOD == "boot" ~ "Bootstrap",
    METHOD == "cart" ~ "CART",
    METHOD == "independent" ~ "Independent",
    METHOD == "parametric" ~ "Parametric",
    TRUE ~ METHOD  # Keep original value if no match
  ))


# Prepare the data by updating the STUDY names
CIO_all_estimates <- CIO_all_estimates %>%
  mutate(STUDY = factor(STUDY)) %>%
  mutate(STUDY = paste("Study", as.numeric(STUDY)))

# Create a base plot function to reuse for each method
plot_violin <- function(data, method_name) {
  ggplot(data[data$METHOD == method_name, ], aes(x = factor(1), y = CI_OVERLAP, fill = STUDY)) +
    geom_violin(alpha = 0.6, trim = TRUE, draw_quantiles = c(0.25, 0.5, 0.75)) +
    theme_minimal() +
    labs(title = paste("Confidence Interval Overlap by Study for", method_name),
         x = "",  # Removed the x-axis label
         y = "CIO",
         fill = "Study") +
    theme(axis.text.x = element_blank(),  # Hide x-axis text
          axis.ticks.x = element_blank(),  # Hide x-axis ticks
          legend.position = "none")  # Hide legend in individual plots
}

# List of unique methods
methods <- unique(CIO_all_estimates$METHOD)

# Generate plots for each method
plots <- lapply(methods, function(method) plot_violin(CIO_all_estimates, method))

# Arrange plots in a 2x2 grid and add a common legend
combined_plot <- ggarrange(plotlist = plots, ncol = 2, nrow = 2, common.legend = TRUE, legend = "right")
print(combined_plot)

##



### CIO MEAN VALUES KEEP
# Rename methods
cio_results <- cio_results %>%
  mutate(Method = case_when(
    Method == "boot" ~ "Bootstrap",
    Method == "cart" ~ "CART",
    Method == "independent" ~ "Independent",
    Method == "parametric" ~ "Parametric",
    TRUE ~ Method  # Keep original value if no match
  ))

ggplot(cio_results, aes(x = Method, y = CIO, color = Study)) +
  geom_boxplot(aes(group = Method), width = 0.5, alpha = 0.3, outlier.shape = NA) +  # Adding box plot
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.5), size = 3) +  # Adding jittered points
  labs(title = "Confidence Interval Overlap Values by Study and Method",
       x = "Method",
       y = "CIO") +
  scale_y_continuous(limits = c(-0.6, 1)) +  # Setting the y-axis limits
  theme_minimal()




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

