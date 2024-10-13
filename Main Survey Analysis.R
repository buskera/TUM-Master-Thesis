library(dplyr)
library(ggplot2)
library(tidyr)
library(knitr)
library(psych)       
library(car)         
library(sandwich)
library(lmtest)
library(interactions)
library(kableExtra)
library(MASS)
library(gt)
library(ggcorrplot)
library(ggfortify)
library(multcomp)
library(emmeans)
library(MVN)
library(nparLD)
library(kableExtra)
library(mice)
library(glmnet)
library(caret)
library(mvabund)
library(boot)
library(plotly)

eval(parse("https://www.soscisurvey.de/mainsurvey/?act=SRP9PInFpZYD4AT3x7leUhxj&vQuality&rScript", encoding="UTF-8"))
data <- ds

#-------------------------#
# Data Cleaning Functions #
#-------------------------#

# Function to filter data by questionnaire category and replace -9 and -1 with NA
filter_by_questnnr <- function(data, category_label) {
  data %>%
    filter(QUESTNNR == category_label) %>%
    mutate(across(everything(), ~ ifelse(. %in% c(-9, -1), NA, .)))
}

# Function to remove participants with more than 20% missing data
remove_na_participants <- function(data, threshold = 0.8) {
  data[rowMeans(is.na(data)) <= (1 - threshold), ]
}

# Function to apply attention check based on a specific column and expected value
apply_attention_check <- function(data, attention_column = "DM16_01", expected_value = 7) {
  data %>%
    filter(!!sym(attention_column) == expected_value)
}

# Function to process survey data
process_survey_data <- function(data, category_label, attention_column = "DM16_01", expected_value = 7) {
  data_filtered <- data %>%
    filter_by_questnnr(category_label)
  cat("Participants after filtering by QUESTNNR (", category_label, "):", nrow(data_filtered), "\n")
  
  data_no_na <- data_filtered %>%
    remove_na_participants()
  cat("Participants after removing NAs:", nrow(data_no_na), "\n")
  
  data_attention <- data_no_na %>%
    apply_attention_check(attention_column, expected_value)
  cat("Participants after attention check:", nrow(data_attention), "\n")
  
  return(data_attention)
}

#-----------------------------#
# Combining the Survey Datasets #
#-----------------------------#

# Process each category (AIDIS, AINONDIS, NONAI)
data_list <- list(
  AIDIS = process_survey_data(data, "AIDIS"),
  AINONDIS = process_survey_data(data, "AINONDIS"),
  NONAI = process_survey_data(data, "NONAI")
)

# Participants after filtering by QUESTNNR ( AIDIS ): 136 
# Participants after removing NAs: 65 
# Participants after attention check: 60 
# Participants after filtering by QUESTNNR ( AINONDIS ): 135 
# Participants after removing NAs: 68 
# Participants after attention check: 52 
# Participants after filtering by QUESTNNR ( NONAI ): 135 
# Participants after removing NAs: 74 
# Participants after attention check: 58 

# Add Disclosure column to each data frame in data_list
data_list$AIDIS$Disclosure <- "AI Disclosed"
data_list$AINONDIS$Disclosure <- "AI Not Disclosed"
data_list$NONAI$Disclosure <- "Non-AI"

# Combine all participant data
combined_participant_data <- bind_rows(data_list$AIDIS, data_list$AINONDIS, data_list$NONAI)

#-------------------------------#
# Participant Level Calculations #
#-------------------------------#
prefixes <- list(
  audible = "AU",
  netflix = "NF"
)
#fix manipulation scoring so that high score = high perceived manipulation
reverse_manipulation <- function(combined_participant_data, prefixes) {
  for (prefix_group in prefixes) {
    for (pre in prefix_group) {
      data <- combined_participant_data %>%
        mutate(
          !!paste0(pre, "05_01") := 8 - !!sym(paste0(pre, "05_01")),
          !!paste0(pre, "05_02") := 8 - !!sym(paste0(pre, "05_02")),
          !!paste0(pre, "05_03") := 8 - !!sym(paste0(pre, "05_03")),
          !!paste0(pre, "05_04") := 8 - !!sym(paste0(pre, "05_04")),
          !!paste0(pre, "05_05") := 8 - !!sym(paste0(pre, "05_05"))
        )
    }
  }
  return(data)
}

combined_participant_data <- reverse_manipulation(combined_participant_data, prefixes$audible)
combined_participant_data <- reverse_manipulation(combined_participant_data, prefixes$netflix)

# Function to calculate averages for metrics
calculate_averages <- function(data, prefix) {
  metrics <- unique(substring(names(data)[grepl(paste0("^", prefix), names(data))], 1, 5))
  
  averages <- lapply(metrics, function(metric) {
    subquestion_columns <- names(data)[grepl(metric, names(data))]
    rowMeans(data[, subquestion_columns], na.rm = TRUE)
  })
  
  averages_df <- as.data.frame(averages)
  colnames(averages_df) <- paste0(metrics, "avg")
  
  return(averages_df)
}

# Apply the function for AI, AU, and NF prefixes
AI_averages <- calculate_averages(combined_participant_data, "AI")
AU_averages <- calculate_averages(combined_participant_data, "AU")
NF_averages <- calculate_averages(combined_participant_data, "NF")

# Combine the averages into one dataset
averages_combined <- bind_cols(AI_averages, AU_averages, NF_averages)

#---------------------------------#
# Add Demographics and Disclosure #
#---------------------------------#
# Select the demographic variables (DM), CASE ID, and Disclosure condition
dm_data <- combined_participant_data %>%
  dplyr::select(CASE, starts_with("DM"), Disclosure)
# Combine averages with DM data, CASE ID, and Disclosure condition
processed_dataset <- bind_cols(dm_data, averages_combined)

#-----------------------------#
# Handling Human Presence #
#-----------------------------#
# Create separate datasets for WithHuman (NF) and WithoutHuman (AU) metrics
au_data <- processed_dataset %>%
  dplyr::select(CASE, starts_with("AU"), Disclosure, starts_with("DM")) %>%
  rename_with(~ sub("AU", "", .)) %>%
  mutate(HumanPresence = "Without Human")

nf_data <- processed_dataset %>%
  dplyr::select(CASE, starts_with("NF"), Disclosure, starts_with("DM")) %>%
  rename_with(~ sub("NF", "", .)) %>%
  mutate(HumanPresence = "With Human")

combined_ad_data <- bind_rows(au_data, nf_data)

#-----------------------------#
# Handling AI Columns #
#-----------------------------#
# AI metric renaming dictionary
AIrename_dict <- list(
  "AI01" = "structural_assurance", 
  "AI02" = "brand_competence", 
  "AI03" = "willingness_to_depend", 
  "AI04" = "optimism", 
  "AI05" = "innovativeness"
)
# DM renaming dictionary
DMrename_dict <- list(
  "DM01" = "gender",
  "DM03" = "age",
  "DM15" = "education",
  "DM08" = "country"
)
ai_data <- processed_dataset %>%
  dplyr::select(CASE, starts_with("AI"), Disclosure, starts_with("DM"))

# Apply renaming
for (code in names(AIrename_dict)) {
  colnames(ai_data) <- gsub(code, AIrename_dict[[code]], colnames(ai_data))  # Ensure full replacement
}
for (code in names(DMrename_dict)) {
  colnames(ai_data) <- gsub(code, DMrename_dict[[code]], colnames(ai_data))  # Ensure full replacement
}

# Remove the original DM columns (before renaming) from the dataset
# Only keep the newly renamed DM columns
ai_data <- ai_data %>%
  dplyr::select(-starts_with("DM"))
# Repeat AI data for each participant to match human data rows
ai_data_repeated <- ai_data[rep(1:nrow(ai_data), each = 2), ]
# Remove CASE and Disclosure columns to prevent duplication
ai_data_transformed <- ai_data_repeated %>%
  dplyr::select(-CASE, -Disclosure)

# Check for overlapping columns and remove them from ai_data_transformed
overlap_cols <- intersect(colnames(combined_ad_data), colnames(ai_data_transformed))
ai_data_transformed <- ai_data_transformed %>%
  dplyr::select(-all_of(overlap_cols))

# Bind the AI data to the combined human data
processed_dataset_with_ai <- bind_cols(combined_ad_data, ai_data_transformed)

#-----------------------------#
# Renaming AU and NF Metrics #
#-----------------------------#
rename_dict <- list(
  "01" = "reliability", "02" = "usefulness", "03" = "affect", 
  "04" = "willingness_to_rely", "05" = "manipulation", "06" = "purchase_intent",
  "08" = "cbba_sincerity", "09" = "cbba_quality", "10" = "engagement",
  "11" = "affective_response", "12" = "advertiser_investment"
)
# Rename AU/NF columns based on rename_dict
for (code in names(rename_dict)) {
  # Replace any number of underscores (using regex pattern "_+") between the code and "avg"
  colnames(processed_dataset_with_ai) <- sub(paste0(code, "_+avg"), paste0(rename_dict[[code]], "_avg"), colnames(processed_dataset_with_ai))
}

#remove unused sentiment metric
processed_dataset <- processed_dataset_with_ai %>%
  dplyr::select(-matches("07_avg")) 
processed_dataset <- processed_dataset %>%
  dplyr::select(-matches("DM08s"), -matches("countrys"), -matches("DM01"), 
                -matches("DM03"), -matches("DM08"), -matches("DM15"), -matches("DM16_01"))

# Calculate the total number of cells in the dataset
total_cells <- prod(dim(processed_dataset))
# Calculate the total number of missing (NA) values in the dataset
missing_values <- sum(is.na(processed_dataset))
# Calculate the non-response rate as the percentage of missing values
non_response_rate <- (missing_values / total_cells) * 100

cat("The non-response rate (percentage of NAs) in the dataset is:", round(non_response_rate, 2), "%\n")
#The non-response rate (percentage of NAs) in the dataset is: 0.56 %

#---------------------------#
# Composite Trust Variable  #
#---------------------------#
trust_subvars <- processed_dataset %>%
  dplyr::select(reliability_avg, usefulness_avg, affect_avg, willingness_to_rely_avg) %>%
  na.omit()

alpha <- alpha(trust_subvars)
print(alpha)

processed_dataset <- processed_dataset %>%
  mutate(
    trust = rowMeans(dplyr::select(., reliability_avg, usefulness_avg, affect_avg, willingness_to_rely_avg), na.rm = TRUE)
  )
#now have one unified trust metric for the dataset

#---------------------------#
# Outliers                  #
#---------------------------#
key_variables <- c("trust", "engagement_avg", "manipulation_avg", "purchase_intent_avg")
# Influential points (potential outlier analysis)
for (var in key_variables) {
  
  # Create a linear model for each key variable
  model <- lm(as.formula(paste(var, "~ Disclosure * HumanPresence")), data = processed_dataset)
  
  par(mfrow=c(2,2))
  # Perform Breusch-Pagan test for heteroskedasticity
  bp_test <- bptest(model)
  cat("\nBreusch-Pagan Test for Heteroskedasticity for", var, "\n")
  print(bp_test)
  
  # Cook's Distance
  cooks_distances <- cooks.distance(model)
  
  # Identify the top 5 influential points based on Cook's Distance
  cat("\nTop Influential Points for", var, "based on Cook's Distance:\n")
  print(sort(cooks_distances, decreasing = TRUE)[1:5])
  
  # Plot Cook's Distance
  plot(cooks_distances, type = "h", main = paste("Cook's Distance for", var), ylab = "Cook's Distance")
  abline(h = 4 / (nrow(processed_dataset) - length(coef(model))), col = "red", lty = 2)  # Cut-off line
  
  # Leverage (hat values) plot
  hat_values <- hatvalues(model)
  plot(hat_values, type = "h", main = paste("Leverage for", var), ylab = "Leverage")
  abline(h = 2 * mean(hat_values), col = "blue", lty = 2)  # Cut-off for high leverage points
}

# List of influential CASEs based on Cook's Distance and after checking don't seem to be authentic responses
influential_cases <- c(1255, 1079)

# Create a new dataset excluding rows where CASE is in the list of influential cases
filtered_dataset <- processed_dataset[!(processed_dataset$CASE %in% influential_cases), ]
clean_dataset <- filtered_dataset
unique_case_dataset <- clean_dataset %>%
  distinct(CASE, .keep_all = TRUE)
# Check remaining sample size of each disclosure condition
disclosure_counts_unique_cases <- table(unique_case_dataset$Disclosure)
print(disclosure_counts_unique_cases)

#---------------------------#
# Covariate Analysis 1      #
#---------------------------#
lm_trust <- lm(trust ~ Disclosure + HumanPresence + engagement_avg + purchase_intent_avg + manipulation_avg + cbba_sincerity_avg +
                 + cbba_quality_avg + affective_response_avg + advertiser_investment_avg +
                 + structural_assurance_avg + brand_competence_avg + 
                 + willingness_to_depend_avg + optimism_avg + innovativeness_avg 
               + gender + age + education, data = clean_dataset)
summary(lm_trust) #significant predictors: human presence, purchase intent, manipulation, cbba quality, cbba sincerity, advertiser investment

lm_engagement <- lm(engagement_avg ~ Disclosure + HumanPresence + trust + purchase_intent_avg + manipulation_avg + cbba_sincerity_avg +
                      + cbba_quality_avg + affective_response_avg + advertiser_investment_avg +
                      + structural_assurance_avg + brand_competence_avg + 
                      + willingness_to_depend_avg + optimism_avg + innovativeness_avg 
                    + gender + age + education, data = clean_dataset)
summary(lm_engagement) #significant predictors: disclosure
#no engagement covariates

lm_manipulation <- lm(manipulation_avg ~ Disclosure + HumanPresence + engagement_avg + purchase_intent_avg + trust + cbba_sincerity_avg +
                        + cbba_quality_avg + affective_response_avg + advertiser_investment_avg +
                        + structural_assurance_avg + brand_competence_avg + 
                        + willingness_to_depend_avg + optimism_avg + innovativeness_avg 
                      + gender + age + education, data = clean_dataset)
summary(lm_manipulation) #significant predictors: trust, advertiser investment

lm_purchase_intent <- lm(purchase_intent_avg ~ Disclosure + HumanPresence + engagement_avg + manipulation_avg + trust + cbba_sincerity_avg +
                           + cbba_quality_avg + affective_response_avg + advertiser_investment_avg +
                           + structural_assurance_avg + brand_competence_avg + 
                           + willingness_to_depend_avg + optimism_avg + innovativeness_avg 
                         + gender + age + education, data = clean_dataset)
summary(lm_purchase_intent) #significant predictors: trust, cbba quality, affective response

#---------------------------#
# Imputation - Handling NAs #
#---------------------------#

print(sum(is.na(clean_dataset))) #43
md_pattern <- md.pattern(clean_dataset)
print(md_pattern)

# Calculate percentage of missing data for each variable
missing_data_summary <- sapply(clean_dataset, function(x) sum(is.na(x)) / length(x) * 100)
missing_data_summary <- data.frame(Variable = names(missing_data_summary), Missing_Percent = missing_data_summary)
print(missing_data_summary)
# Visualize the missing data by variable
ggplot(missing_data_summary, aes(x = reorder(Variable, -Missing_Percent), y = Missing_Percent)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() + 
  labs(title = "Percentage of Missing Data by Variable", y = "Missing Percentage", x = "Variable")

#imputation
imputation_method <- make.method(clean_dataset)

#Perform multiple imputation -> assumption: MAR
#predictive mean matching (pmm) - performed 5 imputations, meaning for each missing value, 5 plausible values were generated, and the imputed dataset reflects averaged values from these multiple imputations.
imputed_data <- mice(clean_dataset, method = imputation_method, m = 5, maxit = 50, seed = 123)
#Number of logged events: 1252 

# Checking the imputation summary
summary(imputed_data)
imputed_data <- complete(imputed_data, "long")

#---------------------------#
# Covariate Analysis 2      #
#---------------------------#
# Reassess covariates after imputation, as imputing values may alter variable relationships.
lm_trust2 <- lm(trust ~ Disclosure + HumanPresence + engagement_avg + purchase_intent_avg + manipulation_avg + cbba_sincerity_avg + cbba_quality_avg + affective_response_avg + advertiser_investment_avg + structural_assurance_avg + brand_competence_avg + + willingness_to_depend_avg + optimism_avg + innovativeness_avg + gender + age + education, data = imputed_data)
summary(lm_trust2) #significant predictors: human presence, purchase intent, manipulation, cbba quality, cbba sincerity, advertiser investment; new: affective response, structural assurance, and optimism
lm_trust_covariates <- lm(trust ~ Disclosure * HumanPresence + purchase_intent_avg + manipulation_avg + cbba_sincerity_avg + cbba_quality_avg + affective_response_avg + advertiser_investment_avg + structural_assurance_avg + optimism_avg, data = imputed_data)

lm_engagement2 <- lm(engagement_avg ~ Disclosure + HumanPresence + trust + purchase_intent_avg + manipulation_avg + cbba_sincerity_avg ++ cbba_quality_avg + affective_response_avg + advertiser_investment_avg ++ structural_assurance_avg + brand_competence_avg + + willingness_to_depend_avg + optimism_avg + innovativeness_avg + gender + age + education, data = imputed_data)
summary(lm_engagement2) #significant predictors: disclosure; new: purchase intent, manipulation, cbba quality, affective response, structural assurance, brand competence, innovativeness, gender, age, and education
lm_engagement_covariates <- lm(engagement_avg ~ Disclosure * HumanPresence + purchase_intent_avg + manipulation_avg + cbba_quality_avg + affective_response_avg+ structural_assurance_avg + brand_competence_avg + innovativeness_avg + gender + education + age, data = imputed_data)

lm_manipulation2 <- lm(manipulation_avg ~ Disclosure + HumanPresence + engagement_avg + purchase_intent_avg + trust + cbba_sincerity_avg ++ cbba_quality_avg + affective_response_avg + advertiser_investment_avg ++ structural_assurance_avg + brand_competence_avg + + willingness_to_depend_avg + optimism_avg + innovativeness_avg + gender + age + education, data = imputed_data)
summary(lm_manipulation2) #significant predictors: trust, advertiser investment; new: disclosure, engagement, purchase intent, cbba sincerity, affective response, brand competence
lm_manipulation_covariates <- lm(manipulation_avg ~ Disclosure * HumanPresence + trust + purchase_intent_avg + engagement_avg + cbba_sincerity_avg + affective_response_avg + brand_competence_avg, data = imputed_data)


lm_purchase_intent2 <- lm(purchase_intent_avg ~ Disclosure + HumanPresence + engagement_avg + manipulation_avg + trust + cbba_sincerity_avg +
                           + cbba_quality_avg + affective_response_avg + advertiser_investment_avg ++ structural_assurance_avg + brand_competence_avg + + willingness_to_depend_avg + optimism_avg + innovativeness_avg + gender + age + education, data = imputed_data)
summary(lm_purchase_intent2) #significant predictors: trust, cbba quality, affective response; new: human presence, engagement, manipulation, advertiser investment, structural assurance, and willingness to depend on
lm_purchase_intent_covariates <- lm(purchase_intent_avg ~ Disclosure * HumanPresence + trust + engagement_avg + manipulation_avg + cbba_quality_avg + advertiser_investment_avg + affective_response_avg+ structural_assurance_avg + willingness_to_depend_avg, data = imputed_data)
#---------------------------#
# Bootstrapping Check       #
#---------------------------#
lm_models <- list(
  trust = lm_trust_covariates,
  engagement_avg = lm_engagement_covariates,
  manipulation_avg = lm_manipulation_covariates,
  purchase_intent_avg = lm_purchase_intent_covariates
)

# Bootstrapping function using the GLM model
bootstrap_analysis <- function(data, lm_model) {
  # Bootstrap function that fits the model on resampled data
  boot_results <- boot(data, function(data, indices) {
    boot_data <- data[indices, ]
    # Try to refit the model using the resampled data and handle errors
    tryCatch({
      model <- update(lm_model, data = boot_data, start = coef(lm_model))  # Use starting values from the original model
      return(coef(model))  # Return the coefficients from the model
    }, error = function(e) {
      # If there's an error, return NA for all coefficients
      return(rep(NA, length(coef(glm_model))))
    })
  }, R = 1000)  # Number of bootstrap samples
  
  return(boot_results)
}

# Re-run bootstrapping for each model
bootstrap_results <- list()
for (var in names(lm_models)) {
  cat("\nBootstrap results for:", var, "\n")
  boot_results <- bootstrap_analysis(imputed_data, lm_models[[var]])
  print(boot_results)
  bootstrap_results[[var]] <- boot_results
}

combine_bootstrap_results <- function(variable, bootstrap_obj, coef_names) {
  #Calculate bias and standard error
  original_estimate <- bootstrap_obj$t0
  bias <- apply(bootstrap_obj$t, 2, mean, na.rm = TRUE) - original_estimate
  std_error <- apply(bootstrap_obj$t, 2, sd, na.rm = TRUE)
  
  #Create data frame for each variable
  data.frame(
    Variable = rep(variable, length(original_estimate)),
    Coefficient = coef_names,
    Original_Estimate = original_estimate,
    Bias = bias,
    Std_Error = std_error
  )
}
# Define coefficient names for each model
trust_coef_names <- c("(Intercept)", "DisclosureAI Not Disclosed", "DisclosureNon-AI", "HumanPresenceWithout Human", "purchase_intent_avg", "manipulation_avg", "cbba_sincerity_avg", "cbba_quality_avg", "affective_response_avg", "advertiser_investment_avg", "structural_assurance_avg", "optimism_avg", "DisclosureAI Not Disclosed:HumanPresenceWithout Human", "DisclosureNon-AI:HumanPresenceWithout Human")
engagement_coef_names <- c("(Intercept)", "DisclosureAI Not Disclosed", "DisclosureNon-AI", "HumanPresenceWithout Human", "purchase_intent_avg", "manipulation_avg", "cbba_quality_avg", "affective_response_avg", "structural_assurance_avg", "brand_competence_avg","innovativeness_avg", "gender", "age", "education", "DisclosureAI Not Disclosed:HumanPresenceWithout Human", "DisclosureNon-AI:HumanPresenceWithout Human")
manipulation_coef_names <- c("(Intercept)", "DisclosureAI Not Disclosed", "DisclosureNon-AI", "HumanPresenceWithout Human", "engagement_avg", "purchase_intent_avg", "trust", "cbba_sincerity_avg", "affective_response_avg", "brand_competence_avg", "DisclosureAI Not Disclosed:HumanPresenceWithout Human", "DisclosureNon-AI:HumanPresenceWithout Human")
purchase_intent_coef_names <- c("(Intercept)", "DisclosureAI Not Disclosed", "DisclosureNon-AI", "HumanPresenceWithout Human", "engagement_avg", "trust", "manipulation_avg", "cbba_quality_avg", "affective_response_avg", "advertiser_investment_avg", "structural_assurance_avg", "willingness_to_depend_avg", "DisclosureAI Not Disclosed:HumanPresenceWithout Human", "DisclosureNon-AI:HumanPresenceWithout Human")
# Combine the results for each model
trust_results_df <- combine_bootstrap_results("trust", bootstrap_results[["trust"]], trust_coef_names)
engagement_results_df <- combine_bootstrap_results("engagement_avg", bootstrap_results[["engagement_avg"]], engagement_coef_names)
manipulation_results_df <- combine_bootstrap_results("manipulation_avg", bootstrap_results[["manipulation_avg"]], manipulation_coef_names)
purchase_intent_results_df <- combine_bootstrap_results("purchase_intent_avg", bootstrap_results[["purchase_intent_avg"]], purchase_intent_coef_names)

# Combine all results into a single data frame
bootstrap_results_df <- rbind(trust_results_df, engagement_results_df, manipulation_results_df)

# Generate the formatted table for each df or the combined df
bootstrap_results_df %>%
  kable("html", col.names = c("Variable", "Coefficient", "Original Estimate", "Bias", "Standard Error"), digits = 4) %>%
  kable_styling(full_width = FALSE, position = "left") %>%
  row_spec(0, bold = TRUE, font_size = 14) %>%
  column_spec(1, bold = TRUE) %>%
  column_spec(3:5, width = "5em") %>%
  print()

unique_case_dataset <- imputed_data %>%
  distinct(CASE, .keep_all = TRUE)
# Check remaining sample size of each disclosure condition now that we have the final dataset
disclosure_counts_unique_cases <- table(unique_case_dataset$Disclosure)
print(disclosure_counts_unique_cases)

# AI Disclosed AI Not Disclosed           Non-AI 
# 60               51                       57 

#---------------------------#
# Descriptive Statistics    #
#---------------------------#
selected_vars <- imputed_data[, c("trust", "engagement_avg", "purchase_intent_avg", "manipulation_avg", "cbba_sincerity_avg", "cbba_quality_avg", "affective_response_avg", "advertiser_investment_avg", "structural_assurance_avg", "brand_competence_avg", "willingness_to_depend_avg", "optimism_avg", "innovativeness_avg", "gender", "age", "education")]
renamed_vars <- c("Trust", "Engagement", "Purchase Intent", "Manipulation", "Advertiser Sincerity", "Advertiser Quality", "Affective Response", "Advertiser Investment", "Structural Assurance", "Brand Competence", "Willingness to Depend", "Optimism", "Innovativeness","Age", "Gender", "Education")

descriptive_stats <- describe(selected_vars)
total_n <- sum(disclosure_counts_unique_cases)

# Update the 'n' column to reflect the correct total sample size
descriptive_stats$n <- rep(total_n, nrow(descriptive_stats))
rownames(descriptive_stats) <- renamed_vars
relevant_stats <- descriptive_stats[, c("n", "mean", "sd", "min", "max", "skew")]

# Output table
kable(relevant_stats, 
      caption = "<div style='text-align: center;'>Descriptive Statistics for Main Survey Variables</div>") %>%
  kable_styling(full_width = F, position = "center") %>%  # Ensure title is centered
  row_spec(0, bold = TRUE, font_size = 14) %>%
  column_spec(1, bold = TRUE) %>%
  column_spec(2:7, width = "6em") %>%
  print()

write.csv(relevant_stats, "Descriptive_Statisticss_Table.csv", row.names = TRUE)

#descriptive statistics table grouped by experimental condition
imputed_data$Condition <- interaction(imputed_data$Disclosure, imputed_data$HumanPresence)
condition_labels <- c("AI Disclosed:With Human (n = 60)", "AI Disclosed:Without Human (n = 60)","AI Not Disclosed:With Human (n = 51)","AI Not Disclosed:Without Human (n = 51)", "Non-AI:Without Human (n = 57)","Non-AI:With Human (n = 57)")
imputed_data$Condition <- factor(imputed_data$Condition, levels = levels(imputed_data$Condition), labels = condition_labels)
# Group the data by Condition and calculate mean and standard deviation for each variable
long_data <- imputed_data %>%
  dplyr::select(Condition, engagement_avg, trust, purchase_intent_avg, manipulation_avg) %>%
  group_by(Condition) %>%
  summarise(
    Trust_Mean = round(mean(trust, na.rm = TRUE), 3), Trust_SD = round(sd(trust, na.rm = TRUE), 3), 
    Engagement_Mean = round(mean(engagement_avg, na.rm = TRUE), 3), Engagement_SD = round(sd(engagement_avg, na.rm = TRUE), 3),
    PurchaseIntent_Mean = round(mean(purchase_intent_avg, na.rm = TRUE), 3), PurchaseIntent_SD = round(sd(purchase_intent_avg, na.rm = TRUE), 3),
    Manipulation_Mean = round(mean(manipulation_avg, na.rm = TRUE), 3), Manipulation_SD = round(sd(manipulation_avg, na.rm = TRUE), 3))
long_data$Condition <- condition_labels
# final output table with custom display names
kable(long_data, 
      caption = "<div style='text-align: center;'>Descriptive Statistics for Experimental Conditions</div>",
      col.names = c("Condition", "Trust Mean", "Trust SD", "Engagement Mean", "Engagement SD",
                    "Purchase Intent Mean", "Purchase Intent SD", "Manipulation Mean", "Manipulation SD"), align = "c") %>%
  kable_styling(full_width = F, position = "center") %>%
  row_spec(0, bold = TRUE, font_size = 14) %>%
  column_spec(1, bold = TRUE) %>%
  print()

write.csv(long_data, "Descriptive_Statistics_Experimental_Conditions.csv", row.names = FALSE)

#---------------------------#
# Overview Correlation      #
#---------------------------#
cor_vars <- imputed_data[, c("trust", "engagement_avg", "purchase_intent_avg", "manipulation_avg", 
                                    "cbba_sincerity_avg", "cbba_quality_avg", "affective_response_avg", 
                                    "advertiser_investment_avg", "structural_assurance_avg", 
                                    "brand_competence_avg", "willingness_to_depend_avg", "optimism_avg", 
                                    "innovativeness_avg", "age", "gender", "education")]
# Generate and visualize the correlation matrix
cor_matrix <- cor(cor_vars, use = "complete.obs")
rownames(cor_matrix) <- renamed_vars
colnames(cor_matrix) <- renamed_vars

# Visualize the correlation matrix
ggcorrplot(cor_matrix, method = "square", type = "upper", tl.col = "black", tl.srt = 45, 
           lab = TRUE, title = "Correlation Matrix for Main Survey Variables")

#VIF Check
print(vif(lm_trust_covariates))
print(vif(lm_engagement_covariates))
print(vif(lm_manipulation_covariates))
print(vif(lm_purchase_intent_covariates))
#Disclosure:Human Presence interaction for each model shows moderate multicollinearity, however this is expected and within acceptable limits (< 10)

#---------------------------#
# Data Assumptions          #
#---------------------------#
assumption_results <- data.frame(
  Variable = character(),
  Shapiro_p = numeric(),
  Breusch_Pagan_p = numeric(),
  Levene_p = numeric(),
  stringsAsFactors = FALSE
)

# Function to check assumptions for each model
check_assumptions <- function(model_name, dv_name, data) {
  cat("\nChecking assumptions for:", model_name, "\n")
  model <- lm(data[[dv_name]] ~ Disclosure * HumanPresence, data = data)
  residuals <- residuals(model) # Normality of residuals
  shapiro_test <- shapiro.test(residuals) # Shapiro-Wilk test for normality
  bp_test <- bptest(model) # Breusch-Pagan test for heteroskedasticity
  levene_test <- leveneTest(data[[dv_name]] ~ Disclosure * HumanPresence, data = data)# Levene's test for homogeneity of variance
  
  # Store the results in the dataframe
  assumption_results <<- rbind(assumption_results, data.frame(
    Variable = model_name,
    Shapiro_p = shapiro_test$p.value,
    Breusch_Pagan_p = bp_test$p.value,
    Levene_p = levene_test$`Pr(>F)`[1]
  ))
  cat("\nAssumptions check completed for:", model_name, "\n")
}

# Loop through key variables and run the assumption checks
for (var_name in key_variables) {
  check_assumptions(var_name, var_name, imputed_data)
}

# Display the assumption results
print(assumption_results, row.names = FALSE)

write.csv(assumption_results, "assumption_Results_Table.csv", row.names = FALSE)
#normality violated for all key variables
#heteroskedasticity violated for all except purchase intent
#all violate homogeneity -> transformation

#---------------------------#
# Transformations           #
#---------------------------#
transform_data <- imputed_data
#log transformations
transform_data$log_trust <- log(transform_data$trust + 1)
transform_data$log_engagement <- log(transform_data$engagement_avg + 1)
transform_data$log_manipulation <- log(transform_data$manipulation_avg + 1)
transform_data$log_purchase_intent <- log(transform_data$purchase_intent_avg + 1)

# Square Root transformations
transform_data$sqrt_trust <- sqrt(transform_data$trust)
transform_data$sqrt_engagement <- sqrt(transform_data$engagement_avg)
transform_data$sqrt_manipulation <- sqrt(transform_data$manipulation_avg)
transform_data$sqrt_purchase_intent <- sqrt(transform_data$purchase_intent_avg)

# Box-Cox transformations
boxcox_trust_model <- lm(trust ~ 1, data = transform_data)
lambda_trust_bc <- boxcox(boxcox_trust_model, plotit = FALSE)$x[which.max(boxcox(boxcox_trust_model, plotit = FALSE)$y)]
transform_data$boxcox_trust <- (transform_data$trust ^ lambda_trust_bc - 1) / lambda_trust_bc

boxcox_engagement_model <- lm(engagement_avg ~ 1, data = transform_data)
lambda_engagement_bc <- boxcox(boxcox_engagement_model, plotit = FALSE)$x[which.max(boxcox(boxcox_engagement_model, plotit = FALSE)$y)]
transform_data$boxcox_engagement <- (transform_data$engagement_avg ^ lambda_engagement_bc - 1) / lambda_engagement_bc

boxcox_manipulation_model <- lm(manipulation_avg ~ 1, data = transform_data)
lambda_manipulation_bc <- boxcox(boxcox_manipulation_model, plotit = FALSE)$x[which.max(boxcox(boxcox_manipulation_model, plotit = FALSE)$y)]
transform_data$boxcox_manipulation <- (transform_data$manipulation_avg ^ lambda_manipulation_bc - 1) / lambda_manipulation_bc

boxcox_purchase_intent_model <- lm(purchase_intent_avg ~ 1, data = transform_data)
lambda_purchase_intent_bc <- boxcox(boxcox_purchase_intent_model, plotit = FALSE)$x[which.max(boxcox(boxcox_purchase_intent_model, plotit = FALSE)$y)]
transform_data$boxcox_purchase_intent <- (transform_data$purchase_intent_avg ^ lambda_purchase_intent_bc - 1) / lambda_purchase_intent_bc

# Fit LM models
lm_log_trust <- lm(log_trust ~ Disclosure * HumanPresence + purchase_intent_avg + manipulation_avg + cbba_sincerity_avg + cbba_quality_avg + affective_response_avg + advertiser_investment_avg + structural_assurance_avg + age, family = Gamma(link = "identity"), data = transform_data)
lm_sqrt_trust <- lm(sqrt_trust ~ Disclosure * HumanPresence + purchase_intent_avg + manipulation_avg + cbba_sincerity_avg + cbba_quality_avg + affective_response_avg + advertiser_investment_avg + structural_assurance_avg + age, family = Gamma(link = "identity"), data = transform_data)
lm_boxcox_trust <- lm(boxcox_trust ~ Disclosure * HumanPresence + purchase_intent_avg + manipulation_avg + cbba_sincerity_avg + cbba_quality_avg + affective_response_avg + advertiser_investment_avg + structural_assurance_avg + age, family = Gamma(link = "identity"), data = transform_data)

lm_log_engagement <- lm(log_engagement ~ Disclosure * HumanPresence + purchase_intent_avg + manipulation_avg + cbba_quality_avg + affective_response_avg+ structural_assurance_avg + brand_competence_avg + innovativeness_avg + gender + education + age, family = Gamma(link = "identity"), data = transform_data)
lm_sqrt_engagement <- lm(sqrt_engagement ~ Disclosure * HumanPresence + purchase_intent_avg + manipulation_avg + cbba_quality_avg + affective_response_avg+ structural_assurance_avg + brand_competence_avg + innovativeness_avg + gender + education + age, family = Gamma(link = "identity"), data = transform_data)
lm_boxcox_engagement <- lm(boxcox_engagement ~ Disclosure * HumanPresence + purchase_intent_avg + manipulation_avg + cbba_quality_avg + affective_response_avg+ structural_assurance_avg + brand_competence_avg + innovativeness_avg + gender + education + age, family = Gamma(link = "identity"), data = transform_data)

lm_log_manipulation <- lm(log_manipulation ~ Disclosure * HumanPresence + trust + purchase_intent_avg + engagement_avg + cbba_sincerity_avg + affective_response_avg + brand_competence_avg, family = Gamma(link = "identity"), data = transform_data)
lm_sqrt_manipulation <- lm(sqrt_manipulation ~ Disclosure * HumanPresence + trust + purchase_intent_avg + engagement_avg + cbba_sincerity_avg + affective_response_avg + brand_competence_avg, family = Gamma(link = "identity"), data = transform_data)
lm_boxcox_manipulation <- lm(boxcox_manipulation ~ Disclosure * HumanPresence + trust + purchase_intent_avg + engagement_avg + cbba_sincerity_avg + affective_response_avg + brand_competence_avg, family = Gamma(link = "identity"), data = transform_data)

lm_log_purchase_intent <- lm(log_purchase_intent ~ Disclosure * HumanPresence + trust + engagement_avg + manipulation_avg + cbba_quality_avg + advertiser_investment_avg + affective_response_avg+ structural_assurance_avg, family = Gamma(link = "identity"), data = transform_data)
lm_sqrt_purchase_intent <- lm(sqrt_purchase_intent ~ Disclosure * HumanPresence + trust + engagement_avg + manipulation_avg + cbba_quality_avg + advertiser_investment_avg + affective_response_avg+ structural_assurance_avg, family = Gamma(link = "identity"), data = transform_data)
lm_boxcox_purchase_intent <- lm(boxcox_purchase_intent ~ Disclosure * HumanPresence + trust + engagement_avg + manipulation_avg + cbba_quality_avg + advertiser_investment_avg + affective_response_avg+ structural_assurance_avg, family = Gamma(link = "identity"), data = transform_data)

# Compare AICs
aic_results <- data.frame(
  Transformation = c("Untransformed", "Log", "Square Root", "Box-Cox",
                     "Untransformed", "Log", "Square Root", "Box-Cox",
                     "Untransformed", "Log", "Square Root", "Box-Cox",
                     "Untransformed", "Log", "Square Root", "Box-Cox"),
  Variable = c(rep("Trust", 4),
               rep("Engagement", 4),
               rep("Manipulation", 4),
               rep("Purchase Intent", 4)),
  AIC = c(AIC(lm_trust), AIC(lm_log_trust), AIC(lm_sqrt_trust), AIC(lm_boxcox_trust),
          AIC(lm_engagement), AIC(lm_log_engagement), AIC(lm_sqrt_engagement), AIC(lm_boxcox_engagement),
          AIC(lm_manipulation), AIC(lm_log_manipulation), AIC(lm_sqrt_manipulation), AIC(lm_boxcox_manipulation),
          AIC(lm_purchase_intent), AIC(lm_log_purchase_intent), AIC(lm_sqrt_purchase_intent), AIC(lm_boxcox_purchase_intent))
)

aic_results <- aic_results %>%
  arrange(Transformation)
# Arrange and display the results, grouped by variable
kable(aic_results, col.names = c("Transformation", "Variable", "AIC"), caption = "AIC Results for Different Transformations by Variable") %>%
  kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped", "hover")) %>%
                  pack_rows(index = table(aic_results$Transformation))
write.csv(aic_results, "aic_Results_Table.csv", row.names = FALSE)

#after analysis -> transform all data using log
final_dataset <- imputed_data
final_dataset$Disclosure <- factor(final_dataset$Disclosure, levels = unique(final_dataset$Disclosure))
final_dataset$HumanPresence <- factor(final_dataset$HumanPresence, levels = unique(final_dataset$HumanPresence))
# Loop through all columns and apply log transformation if the name ends with _avg or trust (all variables violate normality)
for (col in names(final_dataset)) {
  # Check if the column name ends with _avg or trust and if it's numeric
  if (grepl("_avg$", col) | grepl("trust$", col)) {
    if (is.numeric(final_dataset[[col]])) {
      # Apply log transformation (add 1 to avoid log(0) issues)
      final_dataset[[col]] <- log(final_dataset[[col]] + 1)
    }
  }
}

for (var_name in key_variables) {
  check_assumptions(var_name, var_name, final_dataset)
}
print(assumption_results, row.names = FALSE)
#assumptions still violated -> use GLM for modeling
#final dataset is now log transformed

#---------------------------#
# Final Covariate Analysis - Penalized Regression  #
#---------------------------#
response_vars <- c("trust", "engagement_avg", "manipulation_avg", "purchase_intent_avg")

for (var in response_vars) {
  # Remove the response variable from the predictor set to avoid circular prediction
  predictors <- setdiff(c("Disclosure", "HumanPresence", "purchase_intent_avg", "manipulation_avg", 
                          "cbba_sincerity_avg", "cbba_quality_avg", "affective_response_avg", 
                          "advertiser_investment_avg", "structural_assurance_avg", "brand_competence_avg", 
                          "willingness_to_depend_avg", "optimism_avg", "innovativeness_avg", 
                          "gender", "age", "education"), var)
  y <- final_dataset[[var]]
  X <- model.matrix(as.formula(paste(" ~ ", paste(predictors, collapse = " + "))), data = final_dataset)[, -1]  # Matrix of predictors
  print(paste("Checking for:", var))
  set.seed(123)
  trainIndex <- createDataPartition(y, p = 0.8, list = FALSE)
  X_train <- X[trainIndex,]
  X_test <- X[-trainIndex,]
  y_train <- y[trainIndex]
  y_test <- y[-trainIndex]
  # Elastic Net model with cross-validation to find the optimal lambda
  cv_model <- cv.glmnet(X_train, y_train, alpha = 0.5, family = "gaussian")
  best_lambda <- cv_model$lambda.min
  # Fit final Elastic Net model using the best lambda
  final_model <- glmnet(X_train, y_train, alpha = 0.5, lambda = best_lambda)
  # Get non-zero coefficients
  non_zero_coeffs <- coef(final_model)
  # Convert the S4 object to a matrix and filter non-zero coefficients
  non_zero_coeffs <- as.matrix(non_zero_coeffs)
  non_zero_coeffs <- non_zero_coeffs[non_zero_coeffs != 0, , drop = FALSE]  # Only keep non-zero coefficients
  # Get corresponding predictor names
  predictor_names <- rownames(non_zero_coeffs)
  # Combine predictor names with non-zero coefficients
  non_zero_coeffs_with_names <- data.frame(
    Predictor = predictor_names,
    Coefficient = as.vector(non_zero_coeffs)
  )
  print("Non-zero coefficients:")
  print(non_zero_coeffs_with_names)
  # Predict on test data
  predictions <- predict(final_model, s = best_lambda, newx = X_test)
  # Evaluate model performance using Mean Squared Error (MSE)
  mse <- mean((predictions - y_test)^2)
  print(paste("Elastic Net MSE for", var, ":", mse))
}
#use these results to include most efficient covariates in the glm_models below

#---------------------------#
# GLM with Covariates       #
#---------------------------#
#pick family for GLMs -> distributions = Gamma
par(mfrow = c(2, 2))
hist(final_dataset$trust, main = "Trust Distribution", xlab = "Trust")
hist(final_dataset$engagement_avg, main = "Engagement Distribution", xlab = "Engagement")
hist(final_dataset$manipulation_avg, main = "Manipulation Distribution", xlab = "Manipulation")
hist(final_dataset$purchase_intent_avg, main = "Purchase Intent Distribution", xlab = "Purchase Intent")

#center interaction variables (high VIF score), and make sure factor labels are reapplied correctly, and that right factor is the baseline
cat("Contrast settings for Disclosure:\n")
contrasts(final_dataset$Disclosure)
cat("\nContrast settings for HumanPresence:\n")
contrasts(final_dataset$HumanPresence)

#center interaction terms
final_dataset$Disclosure <- factor(final_dataset$Disclosure, levels = c("Non-AI", "AI Not Disclosed", "AI Disclosed"))
final_dataset$HumanPresence <- factor(final_dataset$HumanPresence, levels = c("Without Human", "With Human"))
# Apply contr.treatment coding to compare to the baseline (Non-AI for Disclosure, Without Human for HumanPresence)
contrasts(final_dataset$Disclosure) <- contr.treatment(levels(final_dataset$Disclosure), base = 1)  # Non-AI as baseline
contrasts(final_dataset$HumanPresence) <- contr.treatment(levels(final_dataset$HumanPresence), base = 1)  # Without Human as baseline

#Build univariate GLM models w/ penalized regression covariates (human presence + disclosure included regardless)
#GLMs are very sensitive to influential points so need to check for outliers on model level
fit_glm_with_outlier_removal <- function(formula, data) {
  initial_model <- glm(formula, family = Gamma(link = "log"), data = data)
  # Calculate Cook's Distance
  cooks_distances <- cooks.distance(initial_model)
  # Define a threshold for influential points
  threshold <- 4 / nrow(data)
  # Identify influential observations to remove
  influential_points <- which(cooks_distances >= threshold)
  # Remove influential observations
  data_filtered <- data %>% filter(cooks_distances < threshold)
  final_model <- glm(formula, family = Gamma(link = "log"), data = data_filtered)
  # Return both the final model and the indices of the removed outliers
  return(list(final_model = final_model, removed_outliers = influential_points))
}

# Define a list of dependent variables and their respective formulas
glm_formulas <- list(
  trust = trust ~ Disclosure * HumanPresence + purchase_intent_avg + manipulation_avg + cbba_sincerity_avg + cbba_quality_avg + affective_response_avg + advertiser_investment_avg + structural_assurance_avg + age + education,
  engagement_avg = engagement_avg ~ Disclosure * HumanPresence + purchase_intent_avg + manipulation_avg + cbba_sincerity_avg + cbba_quality_avg + affective_response_avg + advertiser_investment_avg + structural_assurance_avg + innovativeness_avg + gender + age + education,
  manipulation_avg = manipulation_avg ~ Disclosure * HumanPresence + purchase_intent_avg + cbba_sincerity_avg + affective_response_avg + advertiser_investment_avg + structural_assurance_avg + innovativeness_avg + gender + age + education,
  purchase_intent_avg = purchase_intent_avg ~ Disclosure * HumanPresence + manipulation_avg + cbba_sincerity_avg + cbba_quality_avg + affective_response_avg + advertiser_investment_avg + structural_assurance_avg + brand_competence_avg + willingness_to_depend_avg + optimism_avg + innovativeness_avg + gender + age + education
)

glm_models <- list()
removed_outliers <- list()
# Iterate over the formulas, fit the GLM models after addressing outliers, and store them in the list
for (name in names(glm_formulas)) {
  result <- fit_glm_with_outlier_removal(glm_formulas[[name]], final_dataset)
  glm_models[[name]] <- result$final_model
  removed_outliers[[name]] <- result$removed_outliers
}
# Assign the models to their respective variables
glm_trust <- glm_models$trust
glm_engagement <- glm_models$engagement_avg
glm_manipulation <- glm_models$manipulation_avg
glm_purchase_intent <- glm_models$purchase_intent_avg

total_rows <- nrow(final_dataset)
#summary of points removed from each model
for (name in names(removed_outliers)) {
  num_outliers <- length(removed_outliers[[name]])
  percentage_removed <- (num_outliers / total_rows) * 100
  cat("For", name, "model, removed outliers at rows:", removed_outliers[[name]], "\n")
  cat("\nPercentage of dataset removed:", round(percentage_removed, 2), "%\n\n")
}

# Function to add model statistics and format values
add_model_stats <- function(glm_model, table) {
  # Round only the numeric columns (Estimate)
  table$b <- ifelse(!is.na(as.numeric(table$b)), round(as.numeric(table$b), 3), table$b)
  # AIC and Degrees of Freedom
  aic_value <- AIC(glm_model)
  df_residual <- df.residual(glm_model)
  df_total <- length(glm_model$y) - 1
  pseudo_r2 <- calculate_pseudo_r2(glm_model)
  # Round existing table columns
  table$b <- round(table$b, 3)
  table$SE <- round(table$SE, 3)
  table$`t-value` <- round(table$`t-value`, 3)
  table$`CI Lower` <- round(table$`CI Lower`, 3)
  table$`CI Upper` <- round(table$`CI Upper`, 3)
  # Add a divider row
  divider <- data.frame(
    b = "",
    SE = "",
    `t-value` = "",
    `p-value` = "",
    `CI Lower` = "",
    `CI Upper` = "",
    row.names = "Model Statistics"
  )
  # Model fit statistics
  model_fit_stats <- data.frame(
    b = c(round(aic_value, 3), df_residual, df_total, round(pseudo_r2, 3)),
    SE = rep("", 4),
    `t-value` = rep("", 4),
    `p-value` = rep("", 4),
    `CI Lower` = rep("", 4),
    `CI Upper` = rep("", 4),
    row.names = c("AIC", "Residual df", "Total df", "Pseudo R-squared")
  )
  colnames(model_fit_stats) <- colnames(table)
  colnames(divider) <- colnames(table)
  table <- rbind(table, divider)
  table <- rbind(table, model_fit_stats)
  return(table)
}
#functions for processing model values
calculate_pseudo_r2 <- function(glm_model) {
  null_deviance <- glm_model$null.deviance
  residual_deviance <- glm_model$deviance
  r2 <- 1 - (residual_deviance / null_deviance)
  return(r2)
}
make_unique_names <- function(names) {
  unique_names <- make.unique(names)  # This will append .1, .2, etc. to duplicates
  return(unique_names)
}
add_significance <- function(p_value) {
  if (p_value < 0.001) {
    return(paste0(round(p_value, 3), " ***"))
  } else if (p_value < 0.01) {
    return(paste0(round(p_value, 3), " **"))
  } else if (p_value < 0.05) {
    return(paste0(round(p_value, 3), " *"))
  } else {
    return(round(p_value, 3))
  }
}
format_p_values <- function(p_value) {
  if (is.na(p_value)) {
    return("NA")  # Return "NA" if the value is missing
  }
  if (p_value < 0.001) {
    return(paste0(round(p_value, 3), " ***"))
  } else if (p_value < 0.01) {
    return(paste0(round(p_value, 3), " **"))
  } else if (p_value < 0.05) {
    return(paste0(round(p_value, 3), " *"))
  } else {
    return(round(p_value, 3))
  }
}
#model formatting
centering_note <- "Disclosure and HumanPresence variables were centered to reduce multicollinearity in interaction terms."
significance_note <- "p < 0.001: ***, p < 0.01: **, p < 0.05: *"
condition_mapping <- c("(Intercept)" = "(Intercept)","DisclosureAI Not Disclosed" = "AI Not Disclosed","DisclosureAI Disclosed" = "AI Disclosed","HumanPresenceWith Human" = "With Human Presence","purchase_intent_avg" = "Purchase Intent","manipulation_avg" = "Manipulation","cbba_quality_avg" = "Advertiser Quality Commitment","advertiser_investment_avg" = "Perceived Investment (Effort) of Advertiser","cbba_sincerity_avg" = "Advertiser Sincerity","affective_response_avg" = "Affective Response","trust" = "Trust","structural_assurance_avg" = "Structural Assurance","DisclosureAI Not Disclosed:HumanPresenceWith Human" = "AI Not Disclosed * With Human","DisclosureAI Disclosed:HumanPresenceWith Human" = "AI Disclosed * With Human","age" = "Age","willingness_to_depend_avg" = "Willingness to depend on (AI)","innovativeness_avg" = "Innovativeness (of participant)","Disclosure:HumanPresence" = "Disclosure * HumanPresence", "engagement_avg" = "Engagement","brand_competence_avg" = "Perceived Advertiser Competence", "education" = "Education", "gender" = "Gender", "optimism_avg" = "Optimism (of participant)")

# Trust Model
confint_glm_trust <- confint(glm_trust)
summary_trust <- summary(glm_trust)
trust_table <- data.frame(summary_trust$coefficients)
colnames(trust_table) <- c("b", "SE", "t-value", "p-value")
trust_table$`CI Lower` <- confint_glm_trust[, 1]
trust_table$`CI Upper` <- confint_glm_trust[, 2]
mapped_names <- sapply(rownames(trust_table), function(x) {
  if (!is.na(condition_mapping[x])) {
    condition_mapping[x]
  } else {
    x  # If no mapping is found, keep the original name
  }
})
unique_mapped_names <- make_unique_names(mapped_names)
rownames(trust_table) <- unique_mapped_names
trust_table$`p-value` <- as.numeric(trust_table$`p-value`, format_p_values)  # Format p-values
trust_table$`p-value` <- sapply(trust_table$`p-value`, add_significance)  # Add significance stars

trust_table <- add_model_stats(glm_trust, trust_table)

kable(trust_table, digits = 3, escape = FALSE, 
      col.names = c("Estimate", "Std. Error", "t value", "p value", "CI Lower", "CI Upper"),
      align = "c",
      caption = "<div style='text-align: center;'>GLM Summary for Trust</div>") %>% 
  kable_styling(full_width = F, position = "left") %>%
  column_spec(1, bold = TRUE) %>%  # Make the first column bold
  row_spec(0, bold = TRUE, font_size = 14) %>%  # Bold header with larger font
  row_spec(nrow(trust_table) - 4, background = "lightgray") %>%  # Highlight "Model Statistics" row
  row_spec(nrow(trust_table) - 5, hline_after = TRUE, extra_css = "border-bottom: 1px solid") %>%
  row_spec(nrow(trust_table) - 4, hline_after = TRUE, extra_css = "border-bottom: 1px solid") %>%
  footnote(general = significance_note, centering_note, title_format = "bold")

write.csv(trust_table, "GLM_Trust_Table_with_Statistics.csv", row.names = TRUE)

# Engagement Model
confint_glm_engagement <- confint(glm_engagement)
summary_engagement <- summary(glm_engagement)
engagement_table <- data.frame(summary_engagement$coefficients)
colnames(engagement_table) <- c("b", "SE", "t-value", "p-value")
engagement_table$`CI Lower` <- confint_glm_engagement[, 1]
engagement_table$`CI Upper` <- confint_glm_engagement[, 2]
mapped_names <- sapply(rownames(engagement_table), function(x) {
  if (!is.na(condition_mapping[x])) {
    condition_mapping[x]
  } else {
    x  # If no mapping is found, keep the original name
  }
})
unique_mapped_names <- make_unique_names(mapped_names)
rownames(engagement_table) <- unique_mapped_names
engagement_table$`p-value` <- as.numeric(engagement_table$`p-value`, format_p_values)  # Format p-values
engagement_table$`p-value` <- sapply(engagement_table$`p-value`, add_significance)  # Add significance stars

engagement_table <- add_model_stats(glm_engagement, engagement_table)

kable(engagement_table, digits = 3, escape = FALSE, 
      col.names = c("Estimate", "Std. Error", "t value", "p value", "CI Lower", "CI Upper"),
      align = "c",
      caption = "<div style='text-align: center;'>GLM Summary for Engagement</div>") %>% 
  kable_styling(full_width = F, position = "left") %>%
  column_spec(1, bold = TRUE) %>%  # Make the first column bold
  row_spec(0, bold = TRUE, font_size = 14) %>%  # Bold header with larger font
  row_spec(nrow(engagement_table) - 4, background = "lightgray") %>%  # Highlight "Model Statistics" row
  row_spec(nrow(engagement_table) - 5, hline_after = TRUE, extra_css = "border-bottom: 1px solid") %>%
  row_spec(nrow(engagement_table) - 4, hline_after = TRUE, extra_css = "border-bottom: 1px solid") %>%
  footnote(general = significance_note, centering_note, title_format = "bold")

write.csv(engagement_table, "GLM_Engagement_Table_with_Statistics.csv", row.names = TRUE)

# Manipulation Model
confint_glm_manipulation <- confint(glm_manipulation)
summary_manipulation <- summary(glm_manipulation)
manipulation_table <- data.frame(summary_manipulation$coefficients)
colnames(manipulation_table) <- c("b", "SE", "t-value", "p-value")
manipulation_table$`CI Lower` <- confint_glm_manipulation[, 1]
manipulation_table$`CI Upper` <- confint_glm_manipulation[, 2]
mapped_names <- sapply(rownames(manipulation_table), function(x) {
  if (!is.na(condition_mapping[x])) {
    condition_mapping[x]
  } else {
    x  # If no mapping is found, keep the original name
  }
})
unique_mapped_names <- make_unique_names(mapped_names)
rownames(manipulation_table) <- unique_mapped_names
manipulation_table$`p-value` <- as.numeric(manipulation_table$`p-value`, format_p_values)  # Format p-values
manipulation_table$`p-value` <- sapply(manipulation_table$`p-value`, add_significance)  # Add significance stars

manipulation_table <- add_model_stats(glm_manipulation, manipulation_table)

kable(manipulation_table, digits = 3, escape = FALSE, 
      col.names = c("Estimate", "Std. Error", "t value", "p value", "CI Lower", "CI Upper"),
      align = "c",
      caption = "<div style='text-align: center;'>GLM Summary for Manipulation</div>") %>% 
  kable_styling(full_width = F, position = "left") %>%
  column_spec(1, bold = TRUE) %>%  # Make the first column bold
  row_spec(0, bold = TRUE, font_size = 14) %>%  # Bold header with larger font
  row_spec(nrow(manipulation_table) - 4, background = "lightgray") %>%  # Highlight "Model Statistics" row
  row_spec(nrow(manipulation_table) - 5, hline_after = TRUE, extra_css = "border-bottom: 1px solid") %>%
  row_spec(nrow(manipulation_table) - 4, hline_after = TRUE, extra_css = "border-bottom: 1px solid") %>%
  footnote(general = significance_note, centering_note, title_format = "bold")

write.csv(manipulation_table, "GLM_Manipulation_Table_with_Statistics.csv", row.names = TRUE)

# Purchase Intent Model
confint_glm_purchase_intent <- confint(glm_purchase_intent)
summary_purchase_intent <- summary(glm_purchase_intent)
purchase_intent_table <- data.frame(summary_purchase_intent$coefficients)
colnames(purchase_intent_table) <- c("b", "SE", "t-value", "p-value")
purchase_intent_table$`CI Lower` <- confint_glm_purchase_intent[, 1]
purchase_intent_table$`CI Upper` <- confint_glm_purchase_intent[, 2]
mapped_names <- sapply(rownames(purchase_intent_table), function(x) {
  if (!is.na(condition_mapping[x])) {
    condition_mapping[x]
  } else {
    x  # If no mapping is found, keep the original name
  }
})
unique_mapped_names <- make_unique_names(mapped_names)
rownames(purchase_intent_table) <- unique_mapped_names
purchase_intent_table$`p-value` <- as.numeric(purchase_intent_table$`p-value`, format_p_values)  # Format p-values
purchase_intent_table$`p-value` <- sapply(purchase_intent_table$`p-value`, add_significance)  # Add significance stars

purchase_intent_table <- add_model_stats(glm_purchase_intent, purchase_intent_table)

kable(purchase_intent_table, digits = 3, escape = FALSE, 
      col.names = c("Estimate", "Std. Error", "t value", "p value", "CI Lower", "CI Upper"),
      align = "c",
      caption = "<div style='text-align: center;'>GLM Summary for Purchase Intent</div>") %>% 
  kable_styling(full_width = F, position = "left") %>%
  column_spec(1, bold = TRUE) %>%  # Make the first column bol  row_spec(0, bold = TRUE, font_size = 14) %>%  # Bold header with larger font
  row_spec(nrow(purchase_intent_table) - 4, background = "lightgray") %>%  # Highlight "Model Statistics" row
  row_spec(nrow(purchase_intent_table) - 5, hline_after = TRUE, extra_css = "border-bottom: 1px solid") %>%
  row_spec(nrow(purchase_intent_table) - 4, hline_after = TRUE, extra_css = "border-bottom: 1px solid") %>%
  footnote(general = significance_note, centering_note, title_format = "bold")

write.csv(purchase_intent_table, "GLM_PurchaseIntent_Table_with_Statistics.csv", row.names = TRUE)

# ---------------------------#
# GLM with Covariates model Diagnostics#
# ---------------------------#
# check each model for assumptions
robust_se <- coeftest(glm_manipulation, vcov = vcovHC(glm_manipulation, type = "HC0"))  # HC0 or HC1 are common choices
print(robust_se)

par(mfrow = c(1,3))
# Residuals
plot(fitted(glm_engagement), residuals(glm_engagement, type = "deviance"),
     xlab = "Fitted Values", ylab = "Deviance Residuals", 
     main = "Engagement GLM: Deviance Residuals vs Fitted Values")
abline(h = 0, col = "red")

# Normal Q-Q Plot of residuals
qqnorm(residuals(glm_engagement, type = "deviance"), 
       main = "Engagement GLM: Normal Q-Q Plot of Deviance Residuals")
qqline(residuals(glm_engagement, type = "deviance"), col = "red")

# Scale-Location Plot
plot(glm_engagement, which = 3, main = "Engagement GLM: Scale-Location Plot")

# Cook's Distance
plot(glm_purchase_intent, which = 4, main = "Cook's Distance")

# Residuals vs Leverage
plot(glm_purchase_intent, which = 5, main = "Residuals vs Leverage")

#multicollinearity
vif_values <- vif(glm_purchase_intent)
print(vif_values)

#---------------------------#
# GLM w/ Covariates - POST HOC TESTS   #
#---------------------------#
posthoc_summaries <- list(
  "trust_disclosure" = summary(glht(glm_trust, linfct = mcp(Disclosure = "Tukey"))),"trust_humanpresence" = summary(glht(glm_trust, linfct = mcp(HumanPresence = "Tukey"))),
  "engagement_disclosure" = summary(glht(glm_engagement, linfct = mcp(Disclosure = "Tukey"))),"engagement_humanpresence" = summary(glht(glm_engagement, linfct = mcp(HumanPresence = "Tukey"))),
  "manipulation_disclosure" = summary(glht(glm_manipulation, linfct = mcp(Disclosure = "Tukey"))),"manipulation_humanpresence" = summary(glht(glm_manipulation, linfct = mcp(HumanPresence = "Tukey"))),
  "purchaseintent_disclosure" = summary(glht(glm_purchase_intent, linfct = mcp(Disclosure = "Tukey"))),"purchaseintent_humanpresence" = summary(glht(glm_purchase_intent, linfct = mcp(HumanPresence = "Tukey"))))

apply_bh_correction <- function(posthoc_summary, dv_name) {
  pvals <- posthoc_summary$test$pvalues  # Extract p-values
  bh_corrected_pvals <- p.adjust(pvals, method = "BH")  # Apply BH correction
  
  # Print the original and adjusted p-values
  cat("\nBenjamini-Hochberg corrected p-values for", dv_name, ":\n")
  print(data.frame(Original_P = pvals, BH_Corrected_P = bh_corrected_pvals))
}

# Apply BH correction to each post-hoc test
for (dv in names(posthoc_summaries)) {
  apply_bh_correction(posthoc_summaries[[dv]], dv)
}

format_posthoc_table <- function(summary_posthoc, caption_text) {
  posthoc_results <- data.frame(
    Comparison = names(summary_posthoc$test$coefficients),
    Estimate = summary_posthoc$test$coefficients,
    Sigma = summary_posthoc$test$sigma,
    Z_Value = summary_posthoc$test$tstat,
    p_values = summary_posthoc$test$pvalues
  )
  
  # Apply Benjamini-Hochberg correction to p-values
  posthoc_results$p_values <- p.adjust(posthoc_results$p_values, method = "BH")
  
  posthoc_results$p_values <- as.numeric(posthoc_results$p_values)
  posthoc_results$p_values <- sapply(posthoc_results$p_values, format_p_values)
  
  posthoc_kable <- kable(posthoc_results, digits = 3, escape = FALSE, 
                         caption = caption_text) %>% 
    kable_styling(full_width = F, position = "left") %>%
    column_spec(1, bold = TRUE) %>% 
    row_spec(0, bold = TRUE, font_size = 14)
  
  return(posthoc_kable)
}

combined_posthoc_results <- data.frame()

# Loop through the post-hoc summaries and format the results, adding captions as row groups
for (summary_name in names(posthoc_summaries)) {
  parts <- strsplit(summary_name, "_")[[1]]  # Split the names to get the variable and factor
  variable_name <- parts[1]
  factor_name <- parts[2]
  # Create a caption for each post-hoc test based on variable and factor
  caption_text <- paste(variable_name, "by", factor_name)
  # Extract the post-hoc results
  posthoc_results <- data.frame(
    Comparison = names(posthoc_summaries[[summary_name]]$test$coefficients),
    Estimate = posthoc_summaries[[summary_name]]$test$coefficients,
    Sigma = posthoc_summaries[[summary_name]]$test$sigma,
    Z_Value = posthoc_summaries[[summary_name]]$test$tstat,
    p_values = posthoc_summaries[[summary_name]]$test$pvalues)
  # Apply Benjamini-Hochberg correction to p-values
  posthoc_results$p_values <- p.adjust(posthoc_results$p_values, method = "BH")
  posthoc_results$p_values <- as.numeric(posthoc_results$p_values)
  posthoc_results$p_values <- sapply(posthoc_results$p_values, format_p_values)
  posthoc_results$p_values <- as.character(posthoc_results$p_values)
  posthoc_results$Test <- caption_text
  # Combine with previous post-hoc results
  combined_posthoc_results <- bind_rows(combined_posthoc_results, posthoc_results)
}

combined_posthoc_results_sorted <- combined_posthoc_results %>%
  arrange(Test, Comparison)

combined_posthoc_results_clean <- combined_posthoc_results %>%
  dplyr::select(Test, Comparison, Estimate, Sigma, Z_Value, p_values) %>%
  rename(
    "Test" = "Test",
    "Comparison" = "Comparison",
    "Estimate" = "Estimate",
    "Standard Error" = "Sigma",
    "Z Value" = "Z_Value",
    "P Value" = "p_values"
  ) %>%
  mutate_if(is.numeric, ~ round(., 3))  # Round all numeric columns to 3 digits

# Export the cleaned posthoc results table to CSV
write.csv(combined_posthoc_results_clean, "combined_posthoc_results_grouped.csv", row.names = FALSE)

# Use kable to create the table with proper grouping
kable(combined_posthoc_results_sorted %>% dplyr::select(Comparison, Estimate, Sigma, Z_Value, p_values), 
      digits = 3, escape = FALSE, row.names = FALSE, col.names = c("Comparison", "Estimate", "Sigma", "Z Value", "P Value"),
      caption = "<div style='text-align: center;'>Post-hoc Tests for Combined GLMs with Covariates</div>") %>%
  kable_styling(full_width = F, position = "left", font_size = 10) %>%  # Adjust the font size
  pack_rows(index = table(combined_posthoc_results_sorted$Test)) %>%
  footnote(general = "P-values have been adjusted using the Benjamini-Hochberg correction for multiple comparisons.")

#just look at one pairwise comparison
kable(combined_posthoc_results %>% dplyr::filter(Test == "trust by disclosure") %>% dplyr::select(Comparison, Estimate, Sigma, Z_Value, p_values), 
  digits = 3, escape = FALSE, row.names = FALSE, col.names = c("Comparison", "Estimate", "Sigma", "Z Value", "P Value"),
  caption = "<div style='text-align: center;'>Post-hoc Test for Trust GLM by Disclosure</div>"
) %>%
  kable_styling(full_width = F, position = "left")

#---------------------------#
# Manual Simple Slope Analysis#
#---------------------------#
#significant interaction terms in trust and purchase intent glm models
calc_CI <- function(est, se, conf_level = 0.95) {
  z_value <- qnorm((1 + conf_level) / 2)  # Z-value for 95% CI
  lower_CI <- est - z_value * se
  upper_CI <- est + z_value * se
  return(list(lower = lower_CI, upper = upper_CI))
}
# Extract coefficients and standard errors from glm_trust model (for interaction terms)
trust_summary <- summary(glm_trust)$coefficients
est_interaction_trust_not_disclosed <- trust_summary["DisclosureAI Not Disclosed:HumanPresenceWith Human", "Estimate"]
se_interaction_trust_not_disclosed <- trust_summary["DisclosureAI Not Disclosed:HumanPresenceWith Human", "Std. Error"]
t_value_interaction_trust_not_disclosed <- est_interaction_trust_not_disclosed / se_interaction_trust_not_disclosed
p_value_interaction_trust_not_disclosed <- 2 * pt(abs(t_value_interaction_trust_not_disclosed), df = df.residual(glm_trust), lower.tail = FALSE)
ci_interaction_trust_not_disclosed <- calc_CI(est_interaction_trust_not_disclosed, se_interaction_trust_not_disclosed)

est_interaction_trust_disclosed <- trust_summary["DisclosureAI Disclosed:HumanPresenceWith Human", "Estimate"]
se_interaction_trust_disclosed <- trust_summary["DisclosureAI Disclosed:HumanPresenceWith Human", "Std. Error"]
t_value_interaction_trust_disclosed <- est_interaction_trust_disclosed / se_interaction_trust_disclosed
p_value_interaction_trust_disclosed <- 2 * pt(abs(t_value_interaction_trust_disclosed), df = df.residual(glm_trust), lower.tail = FALSE)
ci_interaction_trust_disclosed <- calc_CI(est_interaction_trust_disclosed, se_interaction_trust_disclosed)

# Extract coefficients and standard errors from glm_purchase_intent model
pi_summary <- summary(glm_purchase_intent)$coefficients
est_interaction_pi_not_disclosed <- pi_summary["DisclosureAI Not Disclosed:HumanPresenceWith Human", "Estimate"]
se_interaction_pi_not_disclosed <- pi_summary["DisclosureAI Not Disclosed:HumanPresenceWith Human", "Std. Error"]
t_value_interaction_pi_not_disclosed <- est_interaction_pi_not_disclosed / se_interaction_pi_not_disclosed
p_value_interaction_pi_not_disclosed <- 2 * pt(abs(t_value_interaction_pi_not_disclosed), df = df.residual(glm_purchase_intent), lower.tail = FALSE)
ci_interaction_pi_not_disclosed <- calc_CI(est_interaction_pi_not_disclosed, se_interaction_pi_not_disclosed)

interaction_table_data <- data.frame(
  Variable = c("Trust", "Trust", "Purchase Intent"),
  Interaction = c("AI Not Disclosed x With Human", "AI Disclosed x With Human", 
                  "AI Not Disclosed x With Human"),
  Estimate = c(est_interaction_trust_not_disclosed, est_interaction_trust_disclosed, 
               est_interaction_pi_not_disclosed),
  SE = c(se_interaction_trust_not_disclosed, se_interaction_trust_disclosed, 
         se_interaction_pi_not_disclosed),
  t_value = c(t_value_interaction_trust_not_disclosed, t_value_interaction_trust_disclosed, 
              t_value_interaction_pi_not_disclosed),
  p_value = c(p_value_interaction_trust_not_disclosed, p_value_interaction_trust_disclosed, 
              p_value_interaction_pi_not_disclosed),
  CI_Lower = c(ci_interaction_trust_not_disclosed$lower, ci_interaction_trust_disclosed$lower, 
               ci_interaction_pi_not_disclosed$lower),
  CI_Upper = c(ci_interaction_trust_not_disclosed$upper, ci_interaction_trust_disclosed$upper, 
               ci_interaction_pi_not_disclosed$upper)
)
interaction_table_data$p_value <- as.numeric(interaction_table_data$p_value)
interaction_table_data$p_value <- sapply(interaction_table_data$p_value, format_p_values)
# Display the table using kable
kable_output <- kable(interaction_table_data, digits = 4, escape = FALSE, 
                      col.names = c("Variable", "Interaction", "Estimate", "SE", "t-value", "p-value", "CI Lower", "CI Upper"), 
                      caption = "<div style='text-align: center;'>Interaction Effects Analysis with Confidence Intervals</div>") %>%
  kable_styling(full_width = F, position = "left") %>%
  row_spec(0, bold = TRUE, font_size = 14)

# Output the table
kable_output
write.csv(table_data, "simple_slope_table.csv", row.names = TRUE)

#---------------------------#
# Multivariate GLM w/ Covariates#
#---------------------------#
dependent_vars <- c("trust", "engagement_avg", "manipulation_avg", "purchase_intent_avg")

dependent_data <- mvabund(final_dataset[, dependent_vars])
#include all significant predictors of the univariate models, except for DVs
mv_glm <- manyglm(dependent_data ~ Disclosure * HumanPresence + 
                    cbba_sincerity_avg + cbba_quality_avg +  
                    affective_response_avg + structural_assurance_avg + age + gender + education +
                    innovativeness_avg + willingness_to_depend_avg + optimism_avg + brand_competence_avg, 
                  family = "gamma", data = final_dataset)

mv_glm_summary <- summary(mv_glm, test = "LR", resamp = "pit.trap", nBoot = 100)
print(mv_glm_summary)
#could not include advertiser investment without model crashing, move forward without it
mv_glm_table <- as.data.frame(mv_glm_summary$coefficients)

# Extract df1 and df2
df1 <- mv_glm_summary$df[1]  # Numerator df
df2 <- mv_glm_summary$df[2]  # Denominator df
mv_glm_table$df1 <- df1
mv_glm_table$df2 <- df2
mv_glm_table <- mv_glm_table[, c("LR value", "Pr(>LR)", "df1", "df2")]
mapped_names <- sapply(rownames(mv_glm_table), function(x) {
  if (!is.na(condition_mapping[x])) {
    condition_mapping[x]
  } else {
    x  # If no mapping is found, keep the original name
  }
})
mapped_names <- as.character(mapped_names)
unique_mapped_names <- make_unique_names(mapped_names)
rownames(mv_glm_table) <- unique_mapped_names
mv_glm_table$`Pr(>LR)` <- as.numeric(mv_glm_table$`Pr(>LR)`)
mv_glm_table$`Pr(>LR)` <- sapply(mv_glm_table$`Pr(>LR)`, format_p_values)
colnames(mv_glm_table)
mv_glm_kable <- kable(mv_glm_table, digits = 3, escape = FALSE, 
                      col.names = c("LR value", "p-value", "df1", "df2"), 
                      caption = "<div style='text-align: center;'>Multivariate GLM Summary</div>") %>%
  kable_styling(full_width = F, position = "left") %>%
  row_spec(0, bold = TRUE, font_size = 14) %>%
  footnote(general = "LR: Likelihood Ratio, * indicates p < 0.05", title_format = "bold")
print(mv_glm_kable)
mv_glm_table <- mv_glm_table %>%
  mutate(`LR value` = as.numeric(`LR value`))  # Convert LR value to numeric 

write.csv(mv_glm_table, "mvGLM_Table_with_Statistics.csv", row.names = TRUE)

mv_glm_plot_data <- mv_glm_table %>% 
  mutate(Effect = rownames(mv_glm_table)) %>% 
  dplyr::select(Effect, `LR value`, `Pr(>LR)`)
mv_glm_plot_data <- mv_glm_plot_data %>%
  mutate(`Pr(>LR)` = as.numeric(gsub("[^0-9.]", "", `Pr(>LR)`)))
mv_glm_plot_data <- mv_glm_plot_data %>%
  mutate(Significance = case_when(
    `Pr(>LR)` < 0.01 ~ 'p < 0.01 **',
    `Pr(>LR)` < 0.05 ~ 'p < 0.05 *',
    TRUE ~ 'Not Significant'
  ))

# Convert 'Significance' to a factor so that ggplot treats it as categorical
mv_glm_plot_data$Significance <- factor(mv_glm_plot_data$Significance, levels = c('p < 0.01 **', 'p < 0.05 *', 'Not Significant'))

# Adjust the labels for clarity
mv_glm_plot_data$Effect <- gsub("DisclosureNon-AI:HumanPresenceWith Human", "Non-AI:With Human", mv_glm_plot_data$Effect)
mv_glm_plot_data$Effect <- gsub("DisclosureAI Not Disclosed:HumanPresenceWith Human", "AI Not Disclosed:With Human", mv_glm_plot_data$Effect)
mv_glm_plot_data <- mv_glm_plot_data %>%
  filter(Effect != "(Intercept)")
# Create the bar plot with correct axis adjustments and label positions
p <- ggplot(mv_glm_plot_data, aes(x = reorder(Effect, `LR value`), y = `LR value`, fill = Significance)) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_manual(values = c('p < 0.01 **' = 'steelblue', 'p < 0.05 *' = 'lightblue', 'Not Significant' = 'gray')) +
  coord_flip() +
  labs(
    x = "Effect", 
    y = "Likelihood Ratio (LR) Value", 
    fill = "Significance"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),      # Increase size of x-axis text
    axis.text.y = element_text(size = 15, face = "bold", color = "black"),      # Increase size of y-axis text
    axis.title = element_text(size = 14, face = "bold"),  # Make axis titles larger and bold
    legend.text = element_text(size = 12),      # Increase size of legend text
    legend.title = element_text(size = 14),     # Increase size of legend title
    panel.grid.major = element_line(size = 0.5, linetype = 'dotted', color = 'lightgray'), # Lighten grid lines
    panel.grid.minor = element_blank()          # Remove minor grid lines
  ) +
  geom_text(aes(label = round(`LR value`, 2)), hjust = 0, color = "black", size = 5)

# Set the y-axis limit to go up to 250 to better visualize results
p <- p + scale_y_continuous(limits = c(0, 250))
plotly_plot <- ggplotly(p)
# Save as an interactive HTML file
htmlwidgets::saveWidget(plotly_plot, "LRmetricsplot.html")

# Check assumptions for the model
dependent_vars <- c("trust", "engagement_avg", "manipulation_avg", "purchase_intent_avg")
dependent_data <- final_dataset[, dependent_vars]
colnames(dependent_data) <- c("Trust", "Engagement", "Manipulation", "Purchase Intent")

# Loop through each dependent variable and plot residuals and Q-Q plots
par(mfrow = c(4, 4))  # Set up the plotting grid
for (i in 1:ncol(dependent_data)) {
  # Plot residuals
  plot(mv_glm$residuals[, i], main = paste("Residuals for mvGLM:", colnames(dependent_data)[i]), 
       xlab = "Index", ylab = "Residuals")
  
  # Plot Q-Q plot
  qqnorm(mv_glm$residuals[, i], main = paste("Normal Q-Q Plot for mvGLM:", colnames(dependent_data)[i]))
  qqline(mv_glm$residuals[, i])
  hist(mv_glm$residuals[,i], main = paste("Histogram of Residuals for mvGLM:", colnames(dependent_data)[i]))
}

#check overall residuals
plot(mv_glm$fitted.values, residuals(mv_glm), 
     xlab = "Fitted Values", 
     ylab = "Residuals", 
     main = "mvGLM: Residuals vs Fitted Values")
abline(h = 0, col = "red")

#model fit by variable
AIC(mv_glm)

#---------------------------#
# Survey Demographics       #
#---------------------------#
survey_cleaned <- clean_dataset 

gender_summary <- survey_cleaned %>%
  group_by(CASE) %>%
  summarize(gender = first(gender)) %>%
  group_by(gender) %>%
  summarize(Count = n()) %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 2))
# Map Gender codes to labels
gender_summary$gender <- factor(gender_summary$gender, 
                                levels = c(1, 2, 3, 4), 
                                labels = c("Female", "Male", "Nonbinary", "Prefer not to say"))
# Display gender summary
print("Gender Distribution:")
print(gender_summary)

# Age Summary (by unique CASE)
age_summary <- survey_cleaned %>%
  group_by(CASE) %>%
  summarize(age = first(age)) %>%
  group_by(age) %>%
  summarize(Count = n()) %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 2))
# Map Age codes to labels
age_summary$age <- factor(age_summary$age, 
                          levels = c(1, 2, 3, 4, 5, 6), 
                          labels = c("Younger than 18 years old", "18 to 24 years old", 
                                     "25 to 34 years old", "35 to 44 years old", 
                                     "45 to 54 years old", "Older than 54 years old"))
# Display age summary
print("Age Distribution:")
print(age_summary)

# Education Summary (by unique CASE)
education_summary <- survey_cleaned %>%
  group_by(CASE) %>%
  summarize(education = first(education)) %>%
  group_by(education) %>%
  summarize(Count = n()) %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 2))
# Map Education codes to labels
education_summary$education <- factor(education_summary$education, 
                                      levels = c(1, 2, 3, 4, 5, 6, 7), 
                                      labels = c("Less than high school diploma", 
                                                 "High school diploma or GED", 
                                                 "Some university or vocational training", 
                                                 "Vocational degree (Ausbildung)", 
                                                 "Bachelor's degree", "Master's degree", 
                                                 "Doctorate degree"))
# Display education summary
print("Education Distribution:")
print(education_summary)




