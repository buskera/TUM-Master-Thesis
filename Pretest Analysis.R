library(dplyr)
library(knitr)
library(tidyr)
library(ggplot2)
library(psych)
library(Routliers)
library(patchwork)

# Load survey data, as of 26/8/24
eval(parse("https://www.soscisurvey.de/adpretest/?act=mQ7h6XB9RaBe7XKJReHvGCCW&vQuality&rScript", encoding="UTF-8"))
pretest <- ds

# Define column prefixes for different surveys
prefixes <- list(
  media = c("AU", "NF"),
  finance = c("BF", "CF"),
  retail = c("DR", "SB")
)

# Function to filter, clean, and structure survey data based on column prefixes and QUESTNNR
filter_and_clean_survey_data <- function(data, prefixes, category_label) {
  relevant_columns <- grep(paste0("^(", paste(prefixes, collapse = "|"), ")"), colnames(data), value = TRUE)
  filtered_data <- data %>%
    filter(QUESTNNR == category_label) %>%
    select(all_of(relevant_columns)) %>%
    mutate(across(everything(), ~ ifelse(.x %in% c(-9, -1), NA, .x)))  # Replace -9 and -1 with NA
  return(filtered_data)
}

# Apply the function to each survey version based on QUESTNNR
media_filtered <- filter_and_clean_survey_data(pretest, prefixes$media, "MEDIA")
finance_filtered <- filter_and_clean_survey_data(pretest, prefixes$finance, "FINANCE")
retail_filtered <- filter_and_clean_survey_data(pretest, prefixes$retail, "RETAIL")

# Function to remove participants with more than half NAs
remove_na_participants <- function(data) {
  threshold <- ncol(data) / 2
  data[rowSums(is.na(data)) <= threshold, ]
}

# Apply the function to each filtered dataset
media_filtered <- remove_na_participants(media_filtered) #50 responses
finance_filtered <- remove_na_participants(finance_filtered) #56 responses
retail_filtered <- remove_na_participants(retail_filtered) #55 responses

# Combine all column names to ensure uniform structure
all_columns <- unique(c(colnames(media_filtered), colnames(finance_filtered), colnames(retail_filtered)))

# Function to add missing columns with NA values and reorder
add_missing_columns <- function(data, all_columns) {
  missing_columns <- setdiff(all_columns, colnames(data))
  data[missing_columns] <- NA
  data <- data[, all_columns]
  return(data)
}

# Ensure all datasets have the same structure
media_filtered <- add_missing_columns(media_filtered, all_columns)
finance_filtered <- add_missing_columns(finance_filtered, all_columns)
retail_filtered <- add_missing_columns(retail_filtered, all_columns)

# Reverse code the 09 variables across all survey versions
reverse_familiarity <- function(data, prefixes) {
  for (prefix_group in prefixes) {
    for (pre in prefix_group) {
      data <- data %>%
        mutate(
          !!paste0(pre, "09_01") := 8 - !!sym(paste0(pre, "09_01")),
          !!paste0(pre, "09_02") := 8 - !!sym(paste0(pre, "09_02")),
          !!paste0(pre, "09_03") := 8 - !!sym(paste0(pre, "09_03"))
        )
    }
  }
  return(data)
}

media_filtered <- reverse_familiarity(media_filtered, prefixes$media)
finance_filtered <- reverse_familiarity(finance_filtered, prefixes$finance)
retail_filtered <- reverse_familiarity(retail_filtered, prefixes$retail)

# Combine the filtered datasets
filtered_data <- bind_rows(media_filtered, finance_filtered, retail_filtered) %>%
  distinct() %>% # Remove duplicates
  filter(rowSums(is.na(.)) < ncol(.)) # Remove fully NA rows


#Participant Level analysis
#Calculate average survey score at the participant level (not addressing weights), identify any outliers or unclean data elements

# Function to calculate participant scores as simple averages
calculate_average_scores_for_participants <- function(data) {
  data %>%
    rowwise() %>%
    mutate(average_score = mean(c_across(everything()), na.rm = TRUE)) %>%
    ungroup() %>%
    select(average_score)
}

# Calculate participant scores as simple averages
media_participant_scores <- calculate_average_scores_for_participants(media_filtered)
finance_participant_scores <- calculate_average_scores_for_participants(finance_filtered)
retail_participant_scores <- calculate_average_scores_for_participants(retail_filtered)

# Combine participant scores into a single dataset for plotting
combined_participant_data <- data.frame(
  Score = c(media_participant_scores$average_score, finance_participant_scores$average_score, retail_participant_scores$average_score),
  Industry = factor(rep(c("Media", "Finance", "Retail"), 
                        times = c(nrow(media_participant_scores), nrow(finance_participant_scores), nrow(retail_participant_scores))))
)

# Plot participant-level average scores
ggplot(combined_participant_data, aes(x = Industry, y = Score, fill = Industry)) +
  geom_boxplot() +
  labs(title = "Boxplot of Participant Average Scores by Industry", y = "Average Score", x = "Industry") +
  theme_minimal()
#No outliers detected 

# Summary statistics for participant average scores
participant_summary_stats <- combined_participant_data %>%
  group_by(Industry) %>%
  summarise(
    Mean = mean(Score, na.rm = TRUE),
    Median = median(Score, na.rm = TRUE),
    SD = sd(Score, na.rm = TRUE),
    Min = min(Score, na.rm = TRUE),
    Max = max(Score, na.rm = TRUE)
  )

participant_summary_stats %>%
  kable(caption = "Summary Statistics for Average Scores by Industry")

# |Industry |     Mean|   Median|        SD|      Min|      Max|
#   |:--------|--------:|--------:|---------:|--------:|--------:|
#   |Finance  | 4.840604| 4.735760| 0.5519564| 3.800000| 6.050000|
#   |Media    | 4.723870| 4.754668| 0.5541841| 3.518987| 5.736842|
#   |Retail   | 4.845418| 4.775000| 0.6059165| 3.717949| 6.500000|
  
anova_result <- aov(Score ~ Industry, data = combined_participant_data)
summary(anova_result)
#results: no statistically significant difference in performance affected by industry
# Df Sum Sq Mean Sq F value Pr(>F)
# Industry      2   0.52  0.2600   0.795  0.453
# Residuals   158  51.65  0.3269

#******************************************************************************************
#Industry Level Analysis
#Calculate averages for each metric and aggregate by industry
calculate_averages <- function(data, prefix) {
  total_sums <- data %>%
    summarize(across(starts_with(prefix), ~ sum(.x, na.rm = TRUE)))
  
  valid_respondents <- data %>%
    summarize(across(starts_with(prefix), ~ sum(!is.na(.x))))
  
  final_averages <- total_sums / valid_respondents
  colnames(final_averages) <- paste0(prefix, sprintf("%02d", 1:ncol(final_averages)), "_avg")
  
  return(final_averages)
}

# Calculate averages for each section while maintaining naming convention
final_averages_au <- calculate_averages(filtered_data, "AU")
final_averages_nf <- calculate_averages(filtered_data, "NF")
final_averages_bf <- calculate_averages(filtered_data, "BF")
final_averages_cf <- calculate_averages(filtered_data, "CF")
final_averages_dr <- calculate_averages(filtered_data, "DR")
final_averages_sb <- calculate_averages(filtered_data, "SB")

# Combine all averages into one dataset
final_averages <- bind_cols(
  final_averages_au, final_averages_nf, final_averages_bf, 
  final_averages_cf, final_averages_dr, final_averages_sb
)

# Calculations equally weighted average across metrics for each industry
calculate_scores <- function(final_averages, industry_prefixes) {
  final_averages %>%
    select(starts_with(industry_prefixes[[1]]), starts_with(industry_prefixes[[2]])) %>%
    summarize(
      message_clarity = mean(c(!!sym(paste0(industry_prefixes[1], "01_avg")), !!sym(paste0(industry_prefixes[2], "01_avg"))), na.rm = TRUE),
      aesthetic_appeal = mean(c(!!sym(paste0(industry_prefixes[1], "02_avg")), !!sym(paste0(industry_prefixes[2], "02_avg"))), na.rm = TRUE),
      plausibility = mean(c(!!sym(paste0(industry_prefixes[1], "03_avg")), !!sym(paste0(industry_prefixes[2], "03_avg"))), na.rm = TRUE),
      affective_response = mean(c(!!sym(paste0(industry_prefixes[1], "04_avg")), !!sym(paste0(industry_prefixes[2], "04_avg")),
                                  !!sym(paste0(industry_prefixes[1], "06_avg")), !!sym(paste0(industry_prefixes[2], "06_avg"))), na.rm = TRUE),
      trust_manipulation = mean(c(!!sym(paste0(industry_prefixes[1], "05_avg")), !!sym(paste0(industry_prefixes[2], "05_avg")),
                                  !!sym(paste0(industry_prefixes[1], "07_avg")), !!sym(paste0(industry_prefixes[2], "07_avg"))), na.rm = TRUE),
      ad_clarity = mean(c(!!sym(paste0(industry_prefixes[1], "08_avg")), !!sym(paste0(industry_prefixes[2], "08_avg"))), na.rm = TRUE),
      brand_influence = mean(c(!!sym(paste0(industry_prefixes[1], "09_avg")), !!sym(paste0(industry_prefixes[2], "09_avg")),
                               !!sym(paste0(industry_prefixes[1], "13_avg")), !!sym(paste0(industry_prefixes[2], "13_avg"))), na.rm = TRUE),
      comparability = mean(c(!!sym(paste0(industry_prefixes[1], "12_avg")), !!sym(paste0(industry_prefixes[2], "12_avg"))), na.rm = TRUE)
    ) %>%
    mutate(overall_score = mean(c(message_clarity, aesthetic_appeal, plausibility, affective_response, trust_manipulation, ad_clarity, brand_influence, comparability), na.rm = TRUE))
}

# Calculating scores for each industry
media_scores <- calculate_scores(final_averages, c("AU", "NF"))
finance_scores <- calculate_scores(final_averages, c("BF", "CF"))
retail_scores <- calculate_scores(final_averages, c("DR", "SB"))

# Combine all industry scores into one dataset
industry_scores <- bind_rows(
  media_scores %>% mutate(industry = "Media"),
  finance_scores %>% mutate(industry = "Finance"),
  retail_scores %>% mutate(industry = "Retail")
)

# Display the scores using a table
industry_scores %>%
  select(industry, overall_score) %>%
  pivot_wider(names_from = industry, values_from = overall_score) %>%
  kable(
    caption = "Overall Industry Mean Scores",
    align = 'c'
  )

# Reshape each industry’s scores to long format
media_scores_long <- media_scores %>%
  select(-overall_score) %>%
  pivot_longer(cols = everything(), names_to = "Metric", values_to = "Score")

# Repeat the process for finance_scores and retail_scores
finance_scores_long <- finance_scores %>%
  select(-overall_score) %>%
  pivot_longer(cols = everything(), names_to = "Metric", values_to = "Score")

retail_scores_long <- retail_scores %>%
  select(-overall_score) %>%
  pivot_longer(cols = everything(), names_to = "Metric", values_to = "Score")

# Combine all long-format data into one dataframe
combined_industry_data <- bind_rows(
  media_scores_long %>% mutate(Industry = "Media"),
  finance_scores_long %>% mutate(Industry = "Finance"),
  retail_scores_long %>% mutate(Industry = "Retail")
)

# Now perform the summary statistics
industry_summary_stats <- combined_industry_data %>%
  group_by(Industry) %>%
  summarise(
    Mean = mean(Score, na.rm = TRUE),
    Median = median(Score, na.rm = TRUE),
    SD = sd(Score, na.rm = TRUE),
    Min = min(Score, na.rm = TRUE),
    Max = max(Score, na.rm = TRUE)
  )

industry_summary_stats %>%
  kable(caption = "Summary Statistics for Average Scores by Industry")

# |Industry |     Mean|   Median|        SD|      Min|      Max|
#   |:--------|--------:|--------:|---------:|--------:|--------:|
#   |Finance  | 4.974079| 4.913816| 0.6557719| 4.170000| 5.746122|
#   |Media    | 4.855690| 4.895133| 0.4585943| 4.085852| 5.348214|
#   |Retail   | 5.131348| 5.122727| 0.2581087| 4.809091| 5.456229|
  
anova_result <- aov(Score ~ Industry, data = combined_industry_data)
summary(anova_result)
# results: no statistically significant difference in performance affected by industry
# Df Sum Sq Mean Sq F value Pr(>F)
# Industry     2  0.306  0.1530   0.649  0.533
# Residuals   21  4.949  0.2357  


#******************************************************************************************
#Metric Level Analysis
calculate_metric_scores_for_participants <- function(data, industry_prefixes) {
  data %>%
    rowwise() %>%
    mutate(
      message_clarity = mean(c_across(starts_with(paste0(industry_prefixes[1], "01")) | starts_with(paste0(industry_prefixes[2], "01"))), na.rm = TRUE),
      aesthetic_appeal = mean(c_across(starts_with(paste0(industry_prefixes[1], "02")) | starts_with(paste0(industry_prefixes[2], "02"))), na.rm = TRUE),
      plausibility = mean(c_across(starts_with(paste0(industry_prefixes[1], "03")) | starts_with(paste0(industry_prefixes[2], "03"))), na.rm = TRUE),
      affective_response = mean(c_across(starts_with(paste0(industry_prefixes[1], "04")) | starts_with(paste0(industry_prefixes[2], "04")) |
                                           starts_with(paste0(industry_prefixes[1], "06")) | starts_with(paste0(industry_prefixes[2], "06"))), na.rm = TRUE),
      trust_manipulation = mean(c_across(starts_with(paste0(industry_prefixes[1], "05")) | starts_with(paste0(industry_prefixes[2], "05")) |
                                           starts_with(paste0(industry_prefixes[1], "07")) | starts_with(paste0(industry_prefixes[2], "07"))), na.rm = TRUE),
      ad_clarity = mean(c_across(starts_with(paste0(industry_prefixes[1], "08")) | starts_with(paste0(industry_prefixes[2], "08"))), na.rm = TRUE),
      brand_influence = mean(c_across(starts_with(paste0(industry_prefixes[1], "09")) | starts_with(paste0(industry_prefixes[2], "09")) |
                                        starts_with(paste0(industry_prefixes[1], "13")) | starts_with(paste0(industry_prefixes[2], "13"))), na.rm = TRUE),
      comparability = mean(c_across(starts_with(paste0(industry_prefixes[1], "12")) | starts_with(paste0(industry_prefixes[2], "12"))), na.rm = TRUE)
    ) %>%
    ungroup()
}

# Calculate metric-level participant scores for each industry
media_metric_scores <- calculate_metric_scores_for_participants(media_filtered, c("AU", "NF"))
finance_metric_scores <- calculate_metric_scores_for_participants(finance_filtered, c("BF", "CF"))
retail_metric_scores <- calculate_metric_scores_for_participants(retail_filtered, c("DR", "SB"))

# Combine participant scores into a single dataset for analysis
combined_metric_data <- bind_rows(
  media_metric_scores %>% mutate(Industry = "Media"),
  finance_metric_scores %>% mutate(Industry = "Finance"),
  retail_metric_scores %>% mutate(Industry = "Retail")
)

# Define the metrics to be tested
metrics <- c("message_clarity", "aesthetic_appeal", "plausibility", "affective_response",
             "trust_manipulation", "ad_clarity", "brand_influence", "comparability")

# Loop through each metric and perform ANOVA at the participant level
for (metric in metrics) {
  cat("\nANOVA for:", metric, "\n")
  formula <- as.formula(paste(metric, "~ Industry"))
  anova_result <- aov(formula, data = combined_metric_data)
  print(summary(anova_result))
  
  # Tukey HSD post-hoc test if ANOVA is significant
  if (!is.na(summary(anova_result)[[1]]$`Pr(>F)`[1]) && summary(anova_result)[[1]]$`Pr(>F)`[1] < 0.05) {
    tukey_result <- TukeyHSD(anova_result)
    print(tukey_result)
  } else {
    cat("Tukey HSD not applicable (ANOVA not significant).\n")
  }
}

# Create an empty list to store plots
plot_list <- list()

# Generate plots for each metric and add them to the plot list
for (metric in metrics) {
  plot <- ggplot(combined_metric_data, aes(x = Industry, y = .data[[metric]], fill = Industry)) +
    geom_boxplot() +
    labs(title = paste("Boxplot of", metric, "by Industry"), y = metric, x = "Industry") +
    theme_minimal()
  
  plot_list[[metric]] <- plot
}

# Combine all plots into a grid
combined_plot <- wrap_plots(plot_list, ncol = 2) # Adjust ncol to control the layout

# Display the combined plot
print(combined_plot)
# message clarity, ad clarity and brand influence were significant ****************

# Cronbach's Alpha for reliability check
media_alpha <- psych::alpha(media_filtered)
finance_alpha <- psych::alpha(finance_filtered)
retail_alpha <- psych::alpha(retail_filtered)

cat("Cronbach's alpha for Media Scores: ", media_alpha$total$raw_alpha, "\n") #0.9110646 
cat("Cronbach's alpha for Finance Scores: ", finance_alpha$total$raw_alpha, "\n") #0.9151063 
cat("Cronbach's alpha for Retail Scores: ", retail_alpha$total$raw_alpha, "\n") #0.9282087 


#**********************************************************************************************
# Overall Analysis

# Weights assigned based on statistical significance found in metric analysis. .2 assigned to message clarity, ad clarity and brand influence
weights <- c(
  message_clarity = 0.2,
  aesthetic_appeal = 0.1,
  plausibility = 0.05,
  affective_response = 0.05,
  trust_manipulation = 0.1,
  ad_clarity = 0.2,
  brand_influence = 0.2,
  comparability = 0.1
)


# Define the metric-to-suffix mapping
metric_prefix_mapping <- list(
  "message_clarity" = c("AU01", "NF01", "BF01", "CF01", "DR01", "SB01"),
  "aesthetic_appeal" = c("AU02", "NF02", "BF02", "CF02", "DR02", "SB02"),
  "plausibility" = c("AU03", "NF03", "BF03", "CF03", "DR03", "SB03"),
  "affective_response" = c("AU04", "NF04", "AU06", "NF06", "BF04", "CF04", "BF06", "CF06", "DR04", "SB04"),
  "trust_manipulation" = c("AU05", "NF05", "AU07", "NF07", "BF05", "CF05", "BF07", "CF07", "DR05", "SB05"),
  "ad_clarity" = c("AU08", "NF08"),
  "brand_influence" = c("AU09", "NF09", "AU13", "NF13", "BF09", "CF09", "BF13", "CF13", "DR09", "SB09", "DR13", "SB13"),
  "comparability" = c("AU12", "NF12", "BF12", "CF12", "DR12", "SB12")
)

# Calculate industry scores based on new assigned weights
calculate_weighted_scores <- function(data, prefixes, weights, metric_suffix_mapping) {
  data %>%
    rowwise() %>%
    mutate(
      weighted_score = sum(
        sapply(names(metric_suffix_mapping), function(metric) {
          # Loop through each prefix and suffix combination
          relevant_columns <- unlist(lapply(prefixes, function(prefix) {
            matching_columns <- grep(paste0("^", prefix), colnames(data), value = TRUE)
            selected_columns <- matching_columns[sapply(matching_columns, function(col) {
              any(sapply(metric_suffix_mapping[[metric]], function(suffix) {
                grepl(suffix, col)
              }))
            })]
            return(selected_columns)
          }))
        
          
          # Calculate the mean for the selected relevant columns
          metric_avg <- if (length(relevant_columns) > 0) {
            mean(c_across(all_of(relevant_columns)), na.rm = TRUE)
          } else {
            NA
          }
          
          # Apply the corresponding weight
          weighted_metric <- metric_avg * weights[metric]
          
      
          
          return(weighted_metric)
        }),
        na.rm = TRUE
      )
    ) %>%
    ungroup() %>%
    select(weighted_score)
}

# Calculate average weighted scores for participants in each industry
media_weighted_scores <- calculate_weighted_scores(media_filtered, prefixes$media, weights, metric_prefix_mapping)
finance_weighted_scores <- calculate_weighted_scores(finance_filtered, prefixes$finance, weights, metric_prefix_mapping)
retail_weighted_scores <- calculate_weighted_scores(retail_filtered, prefixes$retail, weights, metric_prefix_mapping)

# Combine industry scores into a single dataset for analysis
combined_weighted_data <- data.frame(
  Score = c(media_weighted_scores$weighted_score, finance_weighted_scores$weighted_score, retail_weighted_scores$weighted_score),
  Industry = factor(rep(c("Media", "Finance", "Retail"), 
                        times = c(nrow(media_weighted_scores), nrow(finance_weighted_scores), nrow(retail_weighted_scores))))
)

# Run ANOVA to compare weighted scores across industries
anova_result <- aov(Score ~ Industry, data = combined_weighted_data)
anova_summary <- summary(anova_result)

# Extract the p-value and F-statistic for interpretation
p_value <- anova_summary[[1]]$`Pr(>F)`[1]
f_statistic <- anova_summary[[1]]$`F value`[1]

cat("\nANOVA Results:\n")
cat("F-statistic: ", f_statistic, "\n") #42.51311 
cat("p-value: ", p_value, "\n") #1.687763e-15 

# Tukey HSD post-hoc test if ANOVA is significant
if (!is.na(p_value) && p_value < 0.05) {
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
} else {
  cat("\nTukey HSD results are not applicable as ANOVA is not significant.\n")
}
#Results: With new weighted structure it is found that Media significantly outperforms both finance and retail, but there is not 
#a significant difference between the performance of finance vs retail

# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = Score ~ Industry, data = combined_weighted_data)
# 
# $Industry
# diff        lwr        upr    p adj
# Media-Finance   0.85115043  0.6045443  1.0977565 0.000000
# Retail-Finance  0.06843347 -0.1792279  0.3160948 0.790504
# Retail-Media   -0.78271696 -1.0233283 -0.5421057 0.000000

# Visualize the weighted scores across industries with boxplot
combined_weighted_data$Industry <- factor(combined_weighted_data$Industry,
                                          levels = c("Finance", "Media", "Retail"),
                                          labels = c("Financial Services", "Entertainment & Media", "Retail"))

# Create the boxplot with improved aesthetics
stats_data <- combined_weighted_data %>%
  group_by(Industry) %>%
  summarise(
    Mean = mean(Score, na.rm = TRUE),
    Median = median(Score, na.rm = TRUE)
  )

# Create the boxplot with larger font sizes and detailed annotations
ggplot(combined_weighted_data, aes(x = Industry, y = Score, fill = Industry)) +
  geom_boxplot(outlier.shape = NA, width = 0.6, alpha = 0.8) +  # Boxplot without outliers
  geom_text(data = stats_data, aes(label = paste0("Mean: ", round(Mean, 2)), y = Mean), 
            vjust = -1.2, size = 4, color = "black") +  # Label the means
  geom_text(data = stats_data, aes(label = paste0("Median: ", round(Median, 2)), y = Median), 
            vjust = 1.5, size = 4, color = "black") +  # Label the medians
  labs(title = "Boxplot of Weighted-Metric Scores by Industry",
       y = "Weighted Score",
       x = "Industry") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),  # Center title and increase font size
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),  # Increase axis text size
    legend.position = "none",  # Remove legend since fill matches the x-axis categories
    panel.grid.major = element_line(color = "grey80", size = 0.2),  # Lighten grid lines for readability
    panel.grid.minor = element_blank()
  ) +
  scale_fill_manual(values = c("#FF6F61", "#6B5B95", "#88B04B"))


# Function to perform sensitivity analysis with adjusted weights
# This is to make sure that the weights assigned based on statistical significance do not overfit the data 
perform_sensitivity_analysis <- function(data, prefixes, original_weights, metric_suffix_mapping, perturbation = 0.1) {
  
  # Function to recalculate weighted scores based on new weights
  recalculate_scores <- function(new_weights) {
    # Calculate participant-level scores
    scores <- calculate_weighted_scores(data, prefixes, new_weights, metric_suffix_mapping)$weighted_score
    # Return the mean score across participants
    return(mean(scores, na.rm = TRUE))
  }
  
  # Store the original mean weighted score
  original_score <- recalculate_scores(original_weights)
  
  # Initialize vectors to store perturbed scores
  high_perturbation_scores <- numeric(length(original_weights))
  low_perturbation_scores <- numeric(length(original_weights))
  
  # Iterate over each metric to perform sensitivity analysis
  for (i in seq_along(original_weights)) {
    metric <- names(original_weights)[i]
    
    # Create perturbed weights
    high_weights <- original_weights
    high_weights[[metric]] <- original_weights[[metric]] * (1 + perturbation)
    
    low_weights <- original_weights
    low_weights[[metric]] <- original_weights[[metric]] * (1 - perturbation)
    
    # Recalculate mean scores with perturbed weights
    high_perturbation_scores[i] <- recalculate_scores(high_weights)
    low_perturbation_scores[i] <- recalculate_scores(low_weights)
  }
  
  # Combine results into a data frame
  sensitivity_results <- data.frame(
    Metric = names(original_weights),
    Original_Score = original_score,
    High_Perturbation_Score = high_perturbation_scores,
    Low_Perturbation_Score = low_perturbation_scores,
    Industry = "Industry_Label" 
  )
  
  return(sensitivity_results)
}

# Perform sensitivity analysis for each industry
media_sensitivity_results <- perform_sensitivity_analysis(media_filtered, prefixes$media, weights, metric_prefix_mapping)
finance_sensitivity_results <- perform_sensitivity_analysis(finance_filtered, prefixes$finance, weights, metric_prefix_mapping)
retail_sensitivity_results <- perform_sensitivity_analysis(retail_filtered, prefixes$retail, weights, metric_prefix_mapping)

# Combine results into one dataset for analysis
combined_sensitivity_data <- bind_rows(
  media_sensitivity_results %>% mutate(Industry = "Media"),
  finance_sensitivity_results %>% mutate(Industry = "Finance"),
  retail_sensitivity_results %>% mutate(Industry = "Retail")
)

# View results
print(combined_sensitivity_data)

#             Metric       Original_Score High_Perturbation_Score Low_Perturbation_Score Industry
# 1     message_clarity       4.703706                4.797682               4.609729    Media
# 2    aesthetic_appeal       4.703706                4.757076               4.650335    Media
# 3        plausibility       4.703706                4.729462               4.677950    Media
# 4  affective_response       4.703706                4.724993               4.682418    Media
# 5  trust_manipulation       4.703706                4.753619               4.653792    Media
# 6          ad_clarity       4.703706                4.797575               4.609837    Media
# 7     brand_influence       4.703706                4.787243               4.620168    Media
# 8       comparability       4.703706                4.752366               4.655045    Media
# 9     message_clarity       3.852555                3.940355               3.764755  Finance
# 10   aesthetic_appeal       3.852555                3.909137               3.795973  Finance
# 11       plausibility       3.852555                3.880395               3.824715  Finance
# 12 affective_response       3.852555                3.875333               3.829777  Finance
# 13 trust_manipulation       3.852555                3.901652               3.803458  Finance
# 14         ad_clarity       3.852555                3.852555               3.852555  Finance
# 15    brand_influence       3.852555                3.947585               3.757525  Finance
# 16      comparability       3.852555                3.898684               3.806427  Finance
# 17    message_clarity       3.920989                4.022031               3.819946   Retail
# 18   aesthetic_appeal       3.920989                3.975489               3.866489   Retail
# 19       plausibility       3.920989                3.948308               3.893669   Retail
# 20 affective_response       3.920989                3.944699               3.897278   Retail
# 21 trust_manipulation       3.920989                3.966151               3.875826   Retail
# 22         ad_clarity       3.920989                3.920989               3.920989   Retail
# 23    brand_influence       3.920989                4.012852               3.829125   Retail
# 24      comparability       3.920989                3.969489               3.872489   Retail


# Visualize the sensitivity analysis results using ggplot
ggplot(combined_sensitivity_data, aes(x = Metric, y = Original_Score, color = Industry)) +
  geom_point(size = 3) +
  geom_line(aes(y = High_Perturbation_Score), linetype = "dashed", size = 1) +
  geom_line(aes(y = Low_Perturbation_Score), linetype = "dotted", size = 1) +
  labs(
    title = "Sensitivity Analysis: Impact of Weight Perturbations on Weighted Scores",
    y = "Weighted Score",
    x = "Metric"
  ) +
  theme_minimal() +
  facet_wrap(~ Industry, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# results show that while there is some variation due to changes in the weighting scheme, 
#it’s not substantial enough to alter the relative performance between metrics within each industry. 
# Media consistently scores higher, followed by Finance and Retail.

#Demographics
pretest_cleaned <- pretest %>%
  mutate(
    DM01 = ifelse(DM01 == -9, NA, DM01), # Gender
    DM03 = ifelse(DM03 == -9, NA, DM03), # Age
    DM15 = ifelse(DM15 == -9, NA, DM15)  # Education
  )

# Calculate proportions for Gender
gender_summary <- pretest_cleaned %>%
  group_by(DM01) %>%
  summarize(Count = n()) %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 2))

# Map Gender codes to labels
gender_summary$DM01 <- factor(gender_summary$DM01, levels = c(1, 2, 3, 4), labels = c("Female", "Male", "Nonbinary", "Prefer not to say"))

# Display gender summary
print("Gender Distribution:")
print(gender_summary)

# DM01              Count Percentage
# <fct>             <int>      <dbl>
#   1 Female              106      13.8 
# 2 Male                 50       6.53
# 3 Nonbinary             2       0.26
# 4 Prefer not to say     1       0.13

# Calculate proportions for Age
age_summary <- pretest_cleaned %>%
  group_by(DM03) %>%
  summarize(Count = n()) %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 2))

# Map Age codes to labels
age_summary$DM03 <- factor(age_summary$DM03, levels = c(1, 2, 3, 4, 5, 6), labels = c(
  "Younger than 18 years old",
  "18 to 24 years old",
  "25 to 34 years old",
  "35 to 44 years old",
  "45 to 54 years old",
  "Older than 54 years old"
))

# Display age summary
print("Age Distribution:")
print(age_summary)

# 1 Younger than 18 years old     2       0.26
# 2 18 to 24 years old           58       7.57
# 3 25 to 34 years old           51       6.66
# 4 35 to 44 years old           16       2.09
# 5 45 to 54 years old           12       1.57
# 6 Older than 54 years old      20       2.61

# Calculate proportions for Education
education_summary <- pretest_cleaned %>%
  group_by(DM15) %>%
  summarize(Count = n()) %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 2))

# Map Education codes to labels
education_summary$DM15 <- factor(education_summary$DM15, levels = c(1, 2, 3, 4, 5, 6, 7), labels = c(
  "Less than high school diploma",
  "High school diploma or GED",
  "Some university or vocational training",
  "Vocational degree (Ausbildung)",
  "Bachelor's degree",
  "Master's degree",
  "Doctorate degree"
))

# Display education summary
print("Education Distribution:")
print(education_summary)

# 1 Less than high school diploma              3       0.39
# 2 High school diploma or GED                 6       0.78
# 3 Some university or vocational training     7       0.91
# 4 Vocational degree (Ausbildung)             2       0.26
# 5 Bachelor's degree                         81      10.6 
# 6 Master's degree                           54       7.05
# 7 Doctorate degree                           6       0.78
