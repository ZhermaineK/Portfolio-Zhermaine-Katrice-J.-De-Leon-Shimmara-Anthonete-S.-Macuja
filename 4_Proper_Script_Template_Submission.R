
library(tidyverse)
library(skimr)


df_time_series <- read_csv("/Users/rhice/Documents/School/DATA SCI/GITHUB PORTFOLIO/PORTFOLIO 4/4_Time_series_Mointoring_data_Group_014.csv")

glimpse(df_time_series)
summary(df_time_series)
df_time_series %>% skim()


### Handling Missing Values
colSums(is.na(df_time_series))

df_time_series_cleaned <- df_time_series %>%
  fill(starts_with("Patient_"), .direction = "down") 

colSums(is.na(df_time_series_cleaned)) 


### Renaming Columns
df_time_series_cleaned <- df_time_series_cleaned %>%
  rename_with(~ tolower(.) %>% str_replace_all("[^[:alnum:]]", "_"))


df_time_series_cleaned <- df_time_series_cleaned %>%
  rename(
    month_name = month, 
    month_num = month_numerical
  )


### Formatting Data Types
glimpse(df_time_series_cleaned)

df_time_series_cleaned <- df_time_series_cleaned %>%
  mutate(
    date = as.Date(month_name, format = "%m/%d/%Y") 
  ) %>%
  select(date, everything()) %>% 
  select(-month_name) 


df_time_series_cleaned <- df_time_series_cleaned %>%
  mutate(
    across(starts_with("patient_"), as.numeric)
  )

glimpse(df_time_series_cleaned)



### Descriptive Statistics
df_time_series_cleaned %>%
  select(where(is.numeric)) %>%
  summary()

df_time_series_cleaned %>%
  skim()



### Data Visualizations

# directory to save plots 
if (!dir.exists("plots")) {
  dir.create("plots")
}

# plots list
plot_list_time_series <- list()


# 1. Line Plot: Average Steps Over Time for all Patients

df_steps_long <- df_time_series_cleaned %>%
  select(date, starts_with("patient_") & ends_with("_avg_steps")) %>%
  pivot_longer(
    cols = starts_with("patient_"),
    names_to = "patient",
    values_to = "avg_steps"
  )

p1_ts_steps_line <- ggplot(df_steps_long, aes(x = date, y = avg_steps, color = patient)) +
  geom_line(lwd = 1) +
  geom_point(alpha = 0.7) +
  labs(
    title = "Average Steps Over Time for Patients",
    x = "Date",
    y = "Average Steps",
    color = "Patient"
  ) +
  theme_minimal() +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p1_ts_steps_line)
plot_list_time_series[["ts_avg_steps_line_plot"]] <- p1_ts_steps_line



# 2. Line Plot: Stress Level Over Time for all Patients
df_stress_long <- df_time_series_cleaned %>%
  select(date, starts_with("patient_") & ends_with("_stress_level")) %>%
  pivot_longer(
    cols = starts_with("patient_"),
    names_to = "patient",
    values_to = "stress_level"
  )

p2_ts_stress_line <- ggplot(df_stress_long, aes(x = date, y = stress_level, color = patient)) +
  geom_line(lwd = 1) +
  geom_point(alpha = 0.7) +
  labs(
    title = "Stress Level Over Time for Patients",
    x = "Date",
    y = "Stress Level",
    color = "Patient"
  ) +
  theme_minimal() +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p2_ts_stress_line)
plot_list_time_series[["ts_stress_level_line_plot"]] <- p2_ts_stress_line




# 3. Line Plot: BMI Over Time for all Patients
df_bmi_long <- df_time_series_cleaned %>%
  select(date, starts_with("patient_") & ends_with("_bmi")) %>%
  pivot_longer(
    cols = starts_with("patient_"),
    names_to = "patient",
    values_to = "bmi"
  )

p3_ts_bmi_line <- ggplot(df_bmi_long, aes(x = date, y = bmi, color = patient)) +
  geom_line(lwd = 1) +
  geom_point(alpha = 0.7) +
  labs(
    title = "BMI Over Time for Patients",
    x = "Date",
    y = "BMI",
    color = "Patient"
  ) +
  theme_minimal() +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p3_ts_bmi_line)
plot_list_time_series[["ts_bmi_line_plot"]] <- p3_ts_bmi_line




# 4. Box Plot: Distribution of Stress Levels Across Patients

p4_ts_stress_boxplot <- ggplot(df_stress_long, aes(x = patient, y = stress_level, fill = patient)) +
  geom_boxplot() +
  labs(
    title = "Distribution of Stress Levels Across Patients",
    x = "Patient",
    y = "Stress Level"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
print(p4_ts_stress_boxplot)
plot_list_time_series[["ts_stress_level_boxplot"]] <- p4_ts_stress_boxplot




# 5. Scatter Plot: Patient 1 Stress Level vs. Patient 1 Average Steps (over time)
p5_ts_p1_stress_steps_scatter <- ggplot(df_time_series_cleaned, aes(x = patient_1_avg_steps, y = patient_1_stress_level)) +
  geom_point(alpha = 0.7, color = "darkred") +
  labs(
    title = "Patient 1: Stress Level vs. Average Steps",
    x = "Average Steps",
    y = "Stress Level"
  ) +
  theme_minimal()
print(p5_ts_p1_stress_steps_scatter)
plot_list_time_series[["ts_p1_stress_steps_scatter"]] <- p5_ts_p1_stress_steps_scatter



# Save all plots in the list as PNG images
for (plot_name in names(plot_list_time_series)) {
  file_path <- paste0("plots/", plot_name, ".png")
  ggsave(file_path, plot_list_time_series[[plot_name]], width = 10, height = 6, units = "in", dpi = 300)
}



### One Advanced Insight or Statistical Test

ts_patient1_steps <- ts(df_time_series_cleaned$patient_1_avg_steps, start = c(2022, 1), frequency = 12)



decomp_patient1_steps <- decompose(ts_patient1_steps, type = "additive")


plot(decomp_patient1_steps)


ccf_result <- ccf(df_time_series_cleaned$patient_1_avg_steps, df_time_series_cleaned$patient_1_stress_level,
                  main = "Cross-Correlation: Patient 1 Steps vs. Stress Level")
print(ccf_result)


