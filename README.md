# TUM-Master-Thesis
This repository contains the R scripts and data analysis workflows used in both the pretest and main survey phases of a study comparing traditional and AI-generated advertisements. This study was conducted by Allison Busker, a master's student under the chair of Digital Marketing at the Technical University of Munich.

For instructions on conducting the analyses, refer to the sections for Traditional Advertisement Analysis, Pretest Analysis, and Main Survey Analysis. 

# Traditional Advertisement Analysis
This section provides an overview of the analysis of visual metrics for traditional advertisements. The analysis evaluates several visual characteristics, including visual complexity, color harmony, edge density, saturation, and contrast for each image.

Required Libraries: imager, dplyr, tibble, gt

Steps for Running the Analysis:
Load Required Libraries: Ensure that you have the necessary libraries installed and loaded in your R environment.

Define Functions: 
check_visual_complexity(): Calculates the visual complexity of the image by dividing its file size by resolution.
check_color_harmony(): Measures the color harmony by calculating the standard deviation of the hue values.
check_edge_density(): Detects edges in the image using gradients and calculates the proportion of edge pixels.
check_saturation(): Computes the average saturation level of the image by converting it to the HSV color space.
check_contrast(): Calculates the contrast by measuring the standard deviation of pixel intensities in the grayscale version of the image.
The analysis employs custom functions to calculate visual metrics such as visual complexity, color harmony, edge density, saturation, and contrast. These functions are defined within the script provided.

Analyze Images: 
To analyze images, specify the paths to the images in the image_paths list and provide corresponding industry labels in the industry_labels list. The script processes each image, computes the visual metrics, and presents the results in a formatted table.

Generate Output Table: 
create_image_html(): Creates HTML tags to display the images in the table.
gt(): Displays the results in a polished, formatted table.
The results will be displayed in a gt table format, showing values for visual complexity, color harmony, edge density, saturation, and contrast for each analyzed image.

View Results: 
After running the analysis, you can view the results in a formatted table, complete with visual representations of the images, using the gt package's functionality.

For more details, please refer to the full Traditional Advertisement Analysis.R script provided in this repository.

# Pretest Analysis
The pretest analysis evaluates initial consumer perceptions of advertisements through several stages: data preparation, participant-level analysis, industry-level analysis, metric-level analysis, and overall performance assessment.

Required Libraries: dplyr, knitr, tidyr, ggplot2, psych, Routliers, patchwork

Steps to Conduct Pretest Analysis:
Load Required Libraries: Ensure that you have the necessary libraries installed and loaded in your R environment.

Load the survey data: load Soscisurvey survey data into R environment via the eval(parse(...)) function

Data Cleaning and Preparation:
filter_and_clean_survey_data(): Filters and structures survey data for each industry (Media, Finance, Retail).
remove_na_participants(): Removes participants with excessive missing data (> 50%)
add_missing_columns(): Ensures consistent data structure across all datasets.
reverse_familiarity(): Reverse codes brand familiarity outputs to maintain consistent scoring.

Participant-Level Analysis:
calculate_average_scores_for_participants(): Calculates the average score for each participant, equally weights each metric of survey.
Generates boxplots using ggplot2 to compare participant scores across industries.
Provides summary statistics to identify any outliers or inconsistencies.

Industry-Level Analysis:
calculate_averages(): Computes average scores for each metric by industry.
Summarizes industry performance and creates visualizations for easy comparison.

Metric-Level Analysis:
calculate_metric_scores_for_participants(): Computes scores for each metric at the participant level.
Performs ANOVA tests to identify significant differences between industries using aov() and TukeyHSD().

Overall Performance Assessment:
calculate_weighted_scores(): Applies a weighted scoring approach to emphasize metrics with significant differences.
perform_sensitivity_analysis(): Conducts sensitivity analysis to validate the robustness of results.

Visualization and Reporting:
Visualizes the outcomes of the analyses with plots and tables.
Summarizes the key insights to support interpretation.

For full code details and step-by-step instructions, please refer to the Pretest Analysis.R file in this repository.

# Main Survey Analysis
This section contains the full workflow for analyzing the main survey data, which compares the effects of traditional and AI-generated advertisements with varying levels of human presence and AI disclosure. The analysis follows a detailed structure for data cleaning, statistical analysis, covariate checks, GLMs, post-hoc tests, and final diagnostics.

Required Libaries: dplyr, ggplott, tidyr, knitr, psych, car, sandwich, lmtest, interactions, kableExtra, MASS, gt, ggcorrplot, ggfortify, multcomp, emmeans, MVN, nparLD, mice, glmnet, caret, mvabund, boot, plotly

Steps to Conduct Main Survey Analysis:
Load Required Libraries: Ensure that you have the necessary libraries installed and loaded in your R environment.

Load the survey data: load Soscisurvey survey data into R environment via the eval(parse(...)) function

Data Cleaning and Preparation:
filter_by_questnnr(): Filters the dataset based on the questionnaire category label (e.g., AIDIS, AINONDIS, NONAI), and replaces missing values (-9, -1) with NA.
remove_na_participants(): Removes participants who have more than 20% missing data.
apply_attention_check(): Applies attention checks based on specific columns and expected values to ensure data quality.
reverse_manipulation(): Reverses the scoring for manipulation variables, ensuring a consistent scoring direction.
Process survey data for three conditions: AI Disclosed, AI Not Disclosed, and Non-AI, ensuring each dataset is filtered, cleaned, and passed through attention checks.

Participant-Level Calculations:
calculate_averages(): Computes the average scores for metrics (trust, engagement, purchase intent, etc.) for each participant.
Furthermore, in section "Handle Human Presence", the data is split for the 'With Human' (NF) and 'Without Human' (AU) conditions and renames columns.

Composite Variable Calculation: Trust Metric
alpha(): Conducts reliability analysis (Cronbach’s Alpha) for the trust metric.
rowMeans(): Aggregates the individual subvariables into a unified trust metric.

Outlier Detection: 
lm(): Builds linear models for key variables (trust, engagement, etc.).
cooks.distance(): Computes Cook's Distance to detect influential points (outliers).
hatvalues(): Identifies high-leverage points in the data.
In this section linear models are built for each dependent variable and initial influential points are identified and removed from dataset if deemed inauthentic.

Data Imputation:
mice(): Performs multiple imputations on the dataset to handle missing values.
Handles data imputation on lm with covariate models and performs specifically PMM imputation for missing data. Multiple imputations are conducted with 5 iterations (m = 5) to generate plausible values for missing data.

Bootstrapping Check:
bootstrap_analysis(): Conducts bootstrapping for each model to validate estimates.
combine_bootstrap_results: Combines the bootstrap results and summarizes the analysis.
Perform bootstrapping check to confirm validity of lm models after performing data imputation.

Assumption Checks:
check_assumptions(): takes the model name, dependent variable name, and the dataset as inputs. It performs assumption checks by fitting a linear model and computing the necessary tests. The results are stored in a dataframe for later review.
Performs shapiro.test() for normality, bp.test() for heteroskedasticity, and leveneTest() for homogeneity of variances tests on dependent variables.

Data Transformations:
log(), sqrt(), and boxcox() transformations applied to each dependent variable.
aic_results: data frame that combines the above transformations with the untransformed models and performs an AIC model fit comparison
Models are re-fit using the transformed data.

Penalized Regression (last covariate analysis):
cv.glmnet():  Conducts Elastic Net regression with cross-validation to find the optimal lambda value.
Function applies penalized regression (Elastic Net) to identify the most relevant covariates and reduce overfitting.
For loop returns the non-zero coefficient metrics for each dependent variable. The best-performing lambda is selected, and non-zero coefficients are included in subsequent GLM models.

Generalized Linear Models:
glm(): Builds the GLMs for each dependent variable using Gamma distribution and log link family. 
fit_glm_with_outlier_removal(): Fits GLMs and removes influential outliers.
add_model_stats(): Functions to format and add statistics to GLMs.
calculate_pseduo_r2(): Functions to format and add statistics to GLMs.
add_significance(): Functions to format and add statistics to GLMs.
format_p_values(): Functions to format and add statistics to GLMs.

GLM - Post Hoc Tests:
apply_bh_correction(): Applies Benjamini-Hochberg correction for multiple comparisons.
format_posthoc_table(): Formats post-hoc test results into a table.
For loop combines all of the posthoc results for the dependent variables and formats the values into a combined table.

Manual Simple Slope Analysis:
calc_CI(): Calculates confidence intervals for interaction terms.
interaction_table_data: Stores the results of interaction effect analysis.
A simple slope analysis is conducted for interaction effects that were significant in the GLMs (Disclosure x Human Presence).

Multivariate GLM (mvGLM):
manyglm(): Builds multivariate GLMs to simultaneously analyze all dependent variables.
A multivariate GLM is used to analyze all dependent variables (trust, engagement, manipulation, purchase intent) together.

Survey Demographics:
Summarize key demographic information (gender, age, education) from the survey dataset.

For more details, please refer to the full Main Survey Analysis.R script provided in this repository.
