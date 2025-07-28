
# Load libraries
library(tidyverse)
library(skimr)


df_demographic <- read_csv("/Users/rhice/Documents/School/DATA SCI/GITHUB PORTFOLIO/PORTFOLIO 2/2_Demographic_Behavioral_data_Group_014.csv")


glimpse(df_demographic)
summary(df_demographic)
df_demographic %>% skim()



# Handling Missing Values and Redundant Columns
colSums(is.na(df_demographic))

df_demographic_cleaned <- df_demographic %>%
  select(
    `Patient ID`,
    Age,
    Sex,
    Weight_kg,
    Height_cm,
    BMI,
    `Region...7`,
    `Socioeconomic...8`, 
    `Education...9`,
    Physical_Activity_Hours_Week,
    Smoking_Status, 
    Drinking_Status, 
    Patient_Satisfaction_Score,
    Health_Literacy_Score
  )

colSums(is.na(df_demographic_cleaned))



# Renaming Columns
df_demographic_cleaned <- df_demographic_cleaned %>%
  rename(
    patient_id = `Patient ID`,
    age = Age,
    sex = Sex,
    weight_kg = Weight_kg,
    height_cm = Height_cm,
    bmi = BMI,
    region = `Region...7`, 
    socioeconomic = `Socioeconomic...8`, 
    education = `Education...9`, 
    physical_activity_hours_week = Physical_Activity_Hours_Week,
    smoking_status = Smoking_Status, 
    drinking_status = Drinking_Status, 
    patient_satisfaction_score = Patient_Satisfaction_Score,
    health_literacy_score = Health_Literacy_Score
  )

### Formatting Data Types
df_demographic_cleaned <- df_demographic_cleaned %>%
  mutate(
    sex = as.factor(sex),
    region = as.factor(region),
    socioeconomic = as.factor(socioeconomic),
    education = as.factor(education),
    smoking_status = as.factor(smoking_status),
    drinking_status = as.factor(drinking_status),
    patient_satisfaction_score = as.factor(patient_satisfaction_score), 
    health_literacy_score = as.factor(health_literacy_score), 
    age = as.numeric(age),
    weight_kg = as.numeric(weight_kg),
    height_cm = as.numeric(height_cm),
    bmi = as.numeric(bmi),
    physical_activity_hours_week = as.numeric(physical_activity_hours_week)
  )

glimpse(df_demographic_cleaned)

## Generate Outputs for the Dataset

### Descriptive Statistics
df_demographic_cleaned %>%
  select(where(is.numeric)) %>%
  summary()

df_demographic_cleaned %>%
  skim()

cat_vars <- c(
  "sex", "region", "socioeconomic", "education",
  "smoking_status", "drinking_status",
  "patient_satisfaction_score", "health_literacy_score"
)
for (var in cat_vars) {
  cat("\n--- Counts for: ", var, "---\n")
  df_demographic_cleaned %>%
    count(.data[[var]]) %>%
    mutate(proportion = n / sum(n)) %>%
    print()
}

# Data Visualizations

# directory to save plots 
if (!dir.exists("plots")) {
  dir.create("plots")
}

# plots list
plot_list_demographic <- list()

# 1. Histogram: Distribution of Age
p1_dem_age_hist <- ggplot(df_demographic_cleaned, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "darkorchid", color = "black") +
  labs(
    title = "Distribution of Patient Age",
    x = "Age (Years)",
    y = "Number of Patients"
  ) +
  theme_minimal()
print(p1_dem_age_hist)
plot_list_demographic[["dem_age_distribution_histogram"]] <- p1_dem_age_hist

# 2. Bar Plot: Distribution of Region
p2_dem_region_bar <- ggplot(df_demographic_cleaned, aes(x = region, fill = region)) +
  geom_bar() +
  labs(
    title = "Patient Count by Region",
    x = "Region",
    y = "Number of Patients"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p2_dem_region_bar)
plot_list_demographic[["dem_region_bar_chart"]] <- p2_dem_region_bar

# 3. Box Plot: BMI by Smoking Status
p3_dem_bmi_smoking_boxplot <- ggplot(df_demographic_cleaned, aes(x = smoking_status, y = bmi, fill = smoking_status)) +
  geom_boxplot() +
  labs(
    title = "BMI Distribution by Smoking Status",
    x = "Smoking Status",
    y = "BMI"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p3_dem_bmi_smoking_boxplot)
plot_list_demographic[["dem_bmi_smoking_boxplot"]] <- p3_dem_bmi_smoking_boxplot

# 4. Scatter Plot: Physical Activity Hours Week vs. Health Literacy Score (jitter for categorical X)
p4_dem_activity_health_scatter <- ggplot(df_demographic_cleaned, aes(x = health_literacy_score, y = physical_activity_hours_week, color = health_literacy_score)) +
  geom_jitter(width = 0.2, alpha = 0.6) + # Jitter to see density for discrete x-axis
  labs(
    title = "Physical Activity Hours vs. Health Literacy Score",
    x = "Health Literacy Score",
    y = "Physical Activity (Hours/Week)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
print(p4_dem_activity_health_scatter)
plot_list_demographic[["dem_activity_health_scatter"]] <- p4_dem_activity_health_scatter

# 5. Bar Plot: Patient Satisfaction Score Distribution
p5_dem_satisfaction_bar <- ggplot(df_demographic_cleaned, aes(x = patient_satisfaction_score, fill = patient_satisfaction_score)) +
  geom_bar() +
  labs(
    title = "Distribution of Patient Satisfaction Scores",
    x = "Patient Satisfaction Score",
    y = "Number of Patients"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
print(p5_dem_satisfaction_bar)
plot_list_demographic[["dem_satisfaction_bar_chart"]] <- p5_dem_satisfaction_bar

# Save all plots in the list
for (plot_name in names(plot_list_demographic)) {
  file_path <- paste0("plots/", plot_name, ".png") 
  ggsave(file_path, plot_list_demographic[[plot_name]], width = 8, height = 5, units = "in", dpi = 300)
}

### One Advanced Insight or Statistical Test

# Chi-square test: Is there an association between Smoking Status and Drinking Status?
contingency_table_smoking_drinking <- table(df_demographic_cleaned$smoking_status, df_demographic_cleaned$drinking_status)
print(contingency_table_smoking_drinking)

if (all(dim(contingency_table_smoking_drinking) > 1) && all(contingency_table_smoking_drinking > 0)) {
  chi_sq_test_smoking_drinking <- chisq.test(contingency_table_smoking_drinking)
  print(chi_sq_test_smoking_drinking)
}
