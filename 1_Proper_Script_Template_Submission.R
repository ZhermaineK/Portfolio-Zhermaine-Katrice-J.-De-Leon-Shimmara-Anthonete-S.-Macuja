
library(tidyverse)
library(skimr)


df_vital <- read_csv("/Users/rhice/Documents/School/DATA SCI/GITHUB PORTFOLIO/1_Vital_signs_diagnosis_data_Group_014.csv")


glimpse(df_vital)
summary(df_vital)
df_vital %>% skim()

### Handling Missing Values
colSums(is.na(df_vital))

df_vital_cleaned <- df_vital %>%
  select(-c(Medication, Hypertension_Legend, Stress_Level_Legend, Sex_Legend, Smoking_status_Legend))

colSums(is.na(df_vital_cleaned))

### Renaming Columns
df_vital_cleaned <- df_vital_cleaned %>%
  rename_with(~ tolower(.) %>% str_replace_all("[^[:alnum:]]", "_"))

### Formatting Data Types
df_vital_cleaned <- df_vital_cleaned %>%
  mutate(
    sex = as.factor(sex),
    hypertension = as.factor(hypertension),
    smoking_status = as.factor(smoking_status),
    elevated_risk = as.factor(elevated_risk),
    stress_level = as.numeric(stress_level),
    bmi = as.numeric(bmi),
    systolic_bp = as.numeric(systolic_bp),
    diastolic_bp = as.numeric(diastolic_bp),
    heart_rate = as.numeric(heart_rate),
    glucose_mg_dl = as.numeric(glucose_mg_dl),
    cholesterol_mg_dl = as.numeric(cholesterol_mg_dl)
  )

glimpse(df_vital_cleaned)



### Descriptive Statistics
df_vital_cleaned %>%
  select(where(is.numeric)) %>%
  summary()

df_vital_cleaned %>%
  skim()

cat_vars <- c("sex", "hypertension", "smoking_status", "elevated_risk")
for (var in cat_vars) {
  cat("\n--- Counts for: ", var, "---\n")
  df_vital_cleaned %>%
    count(.data[[var]]) %>%
    mutate(proportion = n / sum(n)) %>%
    print()
}

### Data Visualizations

# Histogram: Distribution of Age
ggplot(df_vital_cleaned, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "salmon", color = "black") +
  labs(
    title = "Distribution of Patient Age",
    x = "Age",
    y = "Number of Patients"
  ) +
  theme_minimal()

# Box Plot: Systolic Blood Pressure by Sex
ggplot(df_vital_cleaned, aes(x = sex, y = systolic_bp, fill = sex)) +
  geom_boxplot() +
  labs(
    title = "Systolic Blood Pressure Distribution by Sex",
    x = "Sex (0=Female, 1=Male)",
    y = "Systolic Blood Pressure (mmHg)"
  ) +
  scale_fill_manual(values = c("0" = "salmon", "1" = "lightgreen"),
                    labels = c("0" = "Female", "1" = "Male")) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Scatter Plot: BMI vs. Cholesterol_mg/dL, colored by Elevated_Risk
ggplot(df_vital_cleaned, aes(x = bmi, y = cholesterol_mg_dl, color = elevated_risk)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "BMI vs. Cholesterol by Elevated Risk Category",
    x = "BMI",
    y = "Cholesterol (mg/dL)",
    color = "Elevated Risk Category"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Bar Graph: Count of Patients by Elevated_Risk category
ggplot(df_vital_cleaned, aes(x = elevated_risk, fill = elevated_risk)) +
  geom_bar() +
  labs(
    title = "Patient Count by Elevated Risk Category",
    x = "Elevated Risk Category",
    y = "Number of Patients"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Histogram: Distribution of Heart Rate
ggplot(df_vital_cleaned, aes(x = heart_rate)) +
  geom_histogram(binwidth = 5, fill = "lightgreen", color = "black") +
  labs(
    title = "Distribution of Heart Rate",
    x = "Heart Rate (BPM)",
    y = "Number of Patients"
  ) +
  theme_minimal()

### One Advanced Insight or Statistical Test

# Linear Regression: Does age significantly predict systolic blood pressure?
lm_bp_age <- lm(systolic_bp ~ age, data = df_vital_cleaned)
summary(lm_bp_age)

# Visualization of the linear regression
ggplot(df_vital_cleaned, aes(x = age, y = systolic_bp)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", col = "red") +
  labs(
    title = "Systolic Blood Pressure vs. Age with Regression Line",
    x = "Age (Years)",
    y = "Systolic Blood Pressure (mmHg)"
  ) +
  theme_minimal()

# Alternative Advanced Insight: T-test for Cholesterol by Smoking Status
df_smokers_chol <- df_vital_cleaned %>%
  filter(smoking_status %in% c("0", "2"))

if("0" %in% df_smokers_chol$smoking_status & "2" %in% df_smokers_chol$smoking_status) {
  df_smokers_chol$smoking_status <- factor(df_smokers_chol$smoking_status)
  t_test_cholesterol <- t.test(cholesterol_mg_dl ~ smoking_status, data = df_smokers_chol)
  print(t_test_cholesterol)
} 

# Alternative: Chi-square test for Hypertension vs. Elevated Risk
contingency_table <- table(df_vital_cleaned$hypertension, df_vital_cleaned$elevated_risk)
print(contingency_table)

if (all(dim(contingency_table) > 1) && all(contingency_table > 0)) {
  chi_sq_test <- chisq.test(contingency_table)
  print(chi_sq_test)
} 
