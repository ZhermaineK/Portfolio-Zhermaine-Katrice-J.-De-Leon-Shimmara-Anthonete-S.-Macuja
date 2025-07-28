
library(tidyverse)
library(skimr)


df_nutritional <- read_csv("/Users/rhice/Documents/School/DATA SCI/GITHUB PORTFOLIO/PORTFOLIO 3/3_Nutritional_Dietary_data_Group_014.csv")


glimpse(df_nutritional)
summary(df_nutritional)
df_nutritional %>% skim()


colSums(is.na(df_nutritional))


df_nutritional_cleaned <- df_nutritional 



# Renaming Columns

df_nutritional_cleaned <- df_nutritional_cleaned %>%
  rename_with(~ tolower(.) %>% str_replace_all("[^[:alnum:]]", "_"))

### Formatting Data Types
glimpse(df_nutritional_cleaned)

df_nutritional_cleaned <- df_nutritional_cleaned %>%
  mutate(
    body_fat_percent = as.numeric(body_fat_percent),
    muscle_mass_kg = as.numeric(muscle_mass_kg),
    bmi = as.numeric(bmi),
    physical_activity_hours_week = as.numeric(physical_activity_hours_week),
    daily_caloric_intake_kcal = as.numeric(daily_caloric_intake_kcal),
    protein_intake_g = as.numeric(protein_intake_g),
    fat_intake_g = as.numeric(fat_intake_g),
    carbohydrate_intake_g = as.numeric(carbohydrate_intake_g),
    vitamin_c_mg = as.numeric(vitamin_c_mg),
    iron_mg = as.numeric(iron_mg),
    water_intake_ml = as.numeric(water_intake_ml)
  )

glimpse(df_nutritional_cleaned)



# Descriptive Statistics
df_nutritional_cleaned %>%
  select(where(is.numeric)) %>%
  summary()

df_nutritional_cleaned %>%
  skim()



### Data Visualizations

# directory for plots 
if (!dir.exists("plots")) {
  dir.create("plots")
}

# plot list
plot_list_nutritional <- list()

# 1. Histogram: Distribution of Daily Caloric Intake
p1_nut_caloric_hist <- ggplot(df_nutritional_cleaned, aes(x = daily_caloric_intake_kcal)) +
  geom_histogram(binwidth = 250, fill = "lavender", color = "black") +
  labs(
    title = "Distribution of Daily Caloric Intake",
    x = "Daily Caloric Intake (kcal)",
    y = "Number of Patients"
  ) +
  theme_minimal()
print(p1_nut_caloric_hist)
plot_list_nutritional[["nut_caloric_intake_histogram"]] <- p1_nut_caloric_hist

# 2. Histogram: Distribution of Body Fat Percentage
p2_nut_bodyfat_hist <- ggplot(df_nutritional_cleaned, aes(x = body_fat_percent)) +
  geom_histogram(binwidth = 5, fill = "salmon", color = "black") +
  labs(
    title = "Distribution of Body Fat Percentage",
    x = "Body Fat (%)",
    y = "Number of Patients"
  ) +
  theme_minimal()
print(p2_nut_bodyfat_hist)
plot_list_nutritional[["nut_body_fat_histogram"]] <- p2_nut_bodyfat_hist

# 3. Scatter Plot: Daily Caloric Intake vs. Physical Activity Hours Week
p3_nut_cal_activity_scatter <- ggplot(df_nutritional_cleaned, aes(x = physical_activity_hours_week, y = daily_caloric_intake_kcal)) +
  geom_point(alpha = 0.6, color = "purple") +
  labs(
    title = "Daily Caloric Intake vs. Physical Activity Hours/Week",
    x = "Physical Activity (Hours/Week)",
    y = "Daily Caloric Intake (kcal)"
  ) +
  theme_minimal()
print(p3_nut_cal_activity_scatter)
plot_list_nutritional[["nut_caloric_activity_scatter"]] <- p3_nut_cal_activity_scatter

# 4. Scatter Plot: Protein Intake vs. Muscle Mass
p4_nut_protein_muscle_scatter <- ggplot(df_nutritional_cleaned, aes(x = protein_intake_g, y = muscle_mass_kg)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  labs(
    title = "Protein Intake vs. Muscle Mass",
    x = "Protein Intake (g)",
    y = "Muscle Mass (kg)"
  ) +
  theme_minimal()
print(p4_nut_protein_muscle_scatter)
plot_list_nutritional[["nut_protein_muscle_scatter"]] <- p4_nut_protein_muscle_scatter

# 5. Box Plot: BMI by Physical Activity Hours (grouped into categories for visualization)
# Create activity level categories for the box plot
df_nutritional_cleaned_grouped <- df_nutritional_cleaned %>%
  mutate(
    activity_level = case_when(
      physical_activity_hours_week < 5 ~ "Low (<5h)",
      physical_activity_hours_week >= 5 & physical_activity_hours_week < 10 ~ "Moderate (5-10h)",
      TRUE ~ "High (>=10h)"
    ) %>% factor(levels = c("Low (<5h)", "Moderate (5-10h)", "High (>=10h)")) # Order levels
  )

p5_nut_bmi_activity_boxplot <- ggplot(df_nutritional_cleaned_grouped, aes(x = activity_level, y = bmi, fill = activity_level)) +
  geom_boxplot() +
  labs(
    title = "BMI Distribution by Physical Activity Level",
    x = "Physical Activity Level",
    y = "BMI"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
print(p5_nut_bmi_activity_boxplot)
plot_list_nutritional[["nut_bmi_activity_boxplot"]] <- p5_nut_bmi_activity_boxplot


# Save all plots in the list as PNG images
for (plot_name in names(plot_list_nutritional)) {
  file_path <- paste0("plots/", plot_name, ".png")
  ggsave(file_path, plot_list_nutritional[[plot_name]], width = 8, height = 5, units = "in", dpi = 300)
}

### One Advanced Insight or Statistical Test

# Linear Regression: Does daily caloric intake and physical activity predict body fat percentage?
lm_bodyfat <- lm(body_fat_percent ~ daily_caloric_intake_kcal + physical_activity_hours_week, data = df_nutritional_cleaned)
summary(lm_bodyfat)

