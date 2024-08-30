# TUM-Master-Thesis
This repository contains the R scripts and data analysis workflows used in both the pretest and main survey phases of a study comparing traditional and AI-generated advertisements. This study was conducted by Allison Busker, a master's student under the chair of Digital Marketing at the Technical University of Munich.

For instructions on conducting the analyses, refer to the sections for Traditional Advertisement Analysis (line 6), Pretest Analysis (line 28), and Main Survey Analysis (line 74). 

# Traditional Advertisement Analysis
This section provides an overview of the analysis of visual metrics for traditional advertisements. The analysis evaluates several visual characteristics, including visual complexity, color harmony, edge density, saturation, and contrast for each image.

Required Libraries
imager
dplyr
tibble
gt

Steps for Running the Analysis:
Load Required Libraries: Ensure that you have the necessary libraries installed and loaded in your R environment.

Define Functions: The analysis employs custom functions to calculate visual metrics such as visual complexity, color harmony, edge density, saturation, and contrast. These functions are defined within the script provided.

Analyze Images: To analyze images, specify the paths to the images in the image_paths list and provide corresponding industry labels in the industry_labels list. The script processes each image, computes the visual metrics, and presents the results in a formatted table.

Generate Output Table: The results will be displayed in a gt table format, showing values for visual complexity, color harmony, edge density, saturation, and contrast for each analyzed image.

View Results: After running the analysis, you can view the results in a formatted table, complete with visual representations of the images, using the gt package's functionality.

For more details, please refer to the full Traditional Advertisement Analysis.R script provided in this repository.

# Pretest Analysis
The pretest analysis evaluates initial consumer perceptions of advertisements through several stages: data preparation, participant-level analysis, industry-level analysis, metric-level analysis, and overall performance assessment.

Required Libraries
dplyr
knitr
tidyr
ggplot2
psych
Routliers
patchwork

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
