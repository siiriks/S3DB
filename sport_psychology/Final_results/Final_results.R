# load necessary libraries
library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(ggpubr)  
library(dplyr) 

# read in result file
data <- read_excel("sport_psychology/Final_results/final_results.xlsx")


# RMSE RESULTS ------------------------------------------------------------

# define the function for RMSE
calculate_MSE <- function(data, study, method) {
  result <- data |>
    filter(STUDY == study, METHOD == method) |>
    summarise(mean_diff_squared = mean(DIFF_SQUARED, na.rm = TRUE))
  
  return(result$mean_diff_squared)
}

# use RMSE function for all studies and methods

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

# combine RMSE results into a data frame
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



# save to a CSV file
write.csv(results_for_plots, "sport_psychology/Final_results/MSE_results.csv", row.names = FALSE)

# rename studeis to "Study 1" etc.
results_for_plots$Study <- factor(results_for_plots$Study, 
                                  levels = unique(results_for_plots$Study), 
                                  labels = paste("Study", 1:6))


# plot RMSE values
ggplot(results_for_plots, aes(x = Method, y = MSE, color = Study)) +
  geom_boxplot(aes(group = Method), alpha = 0.3, width = 0.4, outlier.shape = NA) +  # Adding box plot with correct alpha value and narrower width
  geom_point(size = 3, position = position_dodge(width = 0.5)) +  # Adding scatter plot with points always in the same order
  scale_y_continuous(trans = 'log10', labels = scales::label_number(accuracy = 0.01)) +  # Scaling the y-axis and setting labels to non-scientific with fewer decimal points
  labs(title = "MSE Values by Study and Method",
       x = "Method",
       y = "MSE (log scale)") +
  theme_minimal()

# CIO RESULTS -------------------------------------------------------------


# function to calculate mean CIO values
mean_CI_overlap <- function(data, study_name, method_name) {
  result <- data |>
    filter(STUDY == study_name, METHOD == method_name) |>
    summarise(avg_CI_OVERLAP = mean(CI_OVERLAP, na.rm = TRUE)) |>
    pull(avg_CI_OVERLAP)
  
  return(result)
}



# Calculate mean CIO values for all studies and methods
S1_CIO_boot <- mean_CI_overlap(data, "Study_1_Chen", "boot")
S1_CIO_independent <- mean_CI_overlap(data, "Study_1_Chen", "independent")
S1_CIO_parametric <- mean_CI_overlap(data, "Study_1_Chen", "parametric")
S1_CIO_cart <- mean_CI_overlap(data, "Study_1_Chen", "cart")


S2_CIO_boot <- mean_CI_overlap(data, "Study_2_Emanuel_1", "boot")
S2_CIO_independent <- mean_CI_overlap(data, "Study_2_Emanuel_1", "independent")
S2_CIO_parametric <- mean_CI_overlap(data, "Study_2_Emanuel_1", "parametric")
S2_CIO_cart <- mean_CI_overlap(data, "Study_2_Emanuel_1", "cart")


S3_CIO_boot <- mean_CI_overlap(data, "Study_3_Emanuel_2", "boot")
S3_CIO_independent <- mean_CI_overlap(data, "Study_3_Emanuel_2", "independent")
S3_CIO_parametric <- mean_CI_overlap(data, "Study_3_Emanuel_2", "parametric")
S3_CIO_cart <- mean_CI_overlap(data, "Study_3_Emanuel_2", "cart")


S4_CIO_boot <- mean_CI_overlap(data, "Study_4_Martin_2024", "boot")
S4_CIO_independent <- mean_CI_overlap(data, "Study_4_Martin_2024", "independent")
S4_CIO_parametric <- mean_CI_overlap(data, "Study_4_Martin_2024", "parametric")
S4_CIO_cart <- mean_CI_overlap(data, "Study_4_Martin_2024", "cart")


S5_CIO_boot <- mean_CI_overlap(data, "Study_5_St_Cyr_2024", "boot")
S5_CIO_independent <- mean_CI_overlap(data, "Study_5_St_Cyr_2024", "independent")
S5_CIO_parametric <- mean_CI_overlap(data, "Study_5_St_Cyr_2024", "parametric")
S5_CIO_cart <- mean_CI_overlap(data, "Study_5_St_Cyr_2024", "cart")


S6_CIO_boot <- mean_CI_overlap(data, "Study_6_Theobald_2022", "boot")
S6_CIO_independent <- mean_CI_overlap(data, "Study_6_Theobald_2022", "independent")
S6_CIO_parametric <- mean_CI_overlap(data, "Study_6_Theobald_2022", "parametric")
S6_CIO_cart <- mean_CI_overlap(data, "Study_6_Theobald_2022", "cart")


# combine mean CIO results into a data frame
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

# save mean CIO results to a CSV file
write.csv(cio_results, "sport_psychology/Final_results/CIO_means.csv", row.names = FALSE)



# rename studies to "Study 1" etc.
cio_results$Study <- factor(cio_results$Study, 
                            levels = unique(cio_results$Study), 
                            labels = paste("Study", 1:6))

## CIO values for all estimates

# create the new dataframe for CIO values for all estimates
CIO_all_estimates <- data |>
  dplyr::filter(METHOD != 'original') |>
  dplyr::select(STUDY, METHOD, CI_OVERLAP)  # Select the required columns

# save as csv
write.csv(CIO_all_estimates, "sport_psychology/Final_results/CIO_all_estimates.csv", row.names = FALSE)


# plot all estimates
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



# get unique study names
studies <- unique(CIO_all_estimates$STUDY)

# create individual plots for each study
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


CIO_all_estimates <- CIO_all_estimates %>%
  mutate(METHOD = case_when(
    METHOD == "boot" ~ "Bootstrap",
    METHOD == "cart" ~ "CART",
    METHOD == "independent" ~ "Independent",
    METHOD == "parametric" ~ "Parametric",
    TRUE ~ METHOD  # Keep original value if no match
  ))


# rename studies
CIO_all_estimates <- CIO_all_estimates %>%
  mutate(STUDY = factor(STUDY)) %>%
  mutate(STUDY = paste("Study", as.numeric(STUDY)))

# create a plot function to reuse for each method
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

# get list of unique methods
methods <- unique(CIO_all_estimates$METHOD)

# generate plots for each method
plots <- lapply(methods, function(method) plot_violin(CIO_all_estimates, method))

# arrange plots in a 2x2 grid
combined_plot <- ggarrange(plotlist = plots, ncol = 2, nrow = 2, common.legend = TRUE, legend = "right")
print(combined_plot)


# rename methods
cio_results <- cio_results %>%
  mutate(Method = case_when(
    Method == "boot" ~ "Bootstrap",
    Method == "cart" ~ "CART",
    Method == "independent" ~ "Independent",
    Method == "parametric" ~ "Parametric",
    TRUE ~ Method  # Keep original value if no match
  ))

# plot mean CIO values
ggplot(cio_results, aes(x = Method, y = CIO, color = Study)) +
  geom_boxplot(aes(group = Method), width = 0.4, alpha = 0.3, outlier.shape = NA) +  # Adding box plot
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.5), size = 3) +  # Adding jittered points
  labs(title = "Confidence Interval Overlap Values by Study and Method",
       x = "Method",
       y = "CIO") +
  scale_y_continuous(limits = c(-0.6, 1)) +  # Setting the y-axis limits
  theme_minimal()




# group by Study and Method and summarize to get min and max CIO values
df_summary <- CIO_all_estimates %>%
  group_by(STUDY, METHOD) %>%
  summarise(
    MIN_CI_OVERLAP = min(CI_OVERLAP, na.rm = TRUE),
    MAX_CI_OVERLAP = max(CI_OVERLAP, na.rm = TRUE),
    MEAN_CI_OVERLAP = mean(CI_OVERLAP, na.rm = TRUE)
  )


write.csv(df_summary, "sport_psychology/Final_results/CIO_ranges.csv", row.names = FALSE)



# ABSOLUTE ERROR RESULTS --------------------------------------------------

# get absolute error values to plot
Absolute_error <- data |>
  dplyr::filter(METHOD != 'original') |>
  dplyr::mutate(DIFFENCE_ORIGINAL = abs(DIFFENCE_ORIGINAL)) |>
  dplyr::select(STUDY, METHOD, ESTIMATE_NAME, DIFFENCE_ORIGINAL)  # Select the required columns


# rename studies
Absolute_error <- Absolute_error %>%
  mutate(STUDY = factor(STUDY, levels = unique(STUDY), labels = paste("Study", 1:length(unique(STUDY)))))

# rename methods
Absolute_error <- Absolute_error %>%
  mutate(METHOD = case_when(
    METHOD == "boot" ~ "Bootstrap",
    METHOD == "cart" ~ "CART",
    METHOD == "independent" ~ "Independent",
    METHOD == "parametric" ~ "Parametric",
    TRUE ~ METHOD  # Keep original value if no match
  ))


# plot absolute errors
ggplot(Absolute_error, aes(x = METHOD, y = DIFFENCE_ORIGINAL)) +
  geom_boxplot(width = 0.5, alpha = 0.6) +
  geom_point(aes(color = METHOD), position = position_jitter(width = 0.2, height = 0), alpha = 0.6, size = 2) +
  theme_minimal() +
  labs(title = "Absolute Error by Study and Method",
       x = "Method",
       y = "Absolute Error",
       color = "Method") +
  facet_wrap(~ STUDY, scales = "free_y", nrow = 3)



