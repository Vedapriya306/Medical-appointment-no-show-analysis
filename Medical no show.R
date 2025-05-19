#Medical Appointments -No Show analysis

# Install necessary libraries if not already installed
install.packages("tidyverse")
install.packages("lubridate")

# Load libraries
library(tidyverse)   # For data manipulation and plotting
library(lubridate)   # For date handling
library(scales)
library(writexl)

# Read the CSV file (update the path if needed)
appointments <- read_csv("C:/Users/srive/Downloads/archive/KaggleV2-May-2016.csv")

# Rename columns for easier usage and fix typo in 'Hipertension'
appointments <- appointments %>%
  rename(
    patient_id = PatientId,
    appointment_id = AppointmentID,
    gender = Gender,
    scheduled_day = ScheduledDay,
    appointment_day = AppointmentDay,
    age = Age,
    neighborhood = Neighbourhood,
    scholarship = Scholarship,
    Hypertension = Hipertension, # Correct spelling
    diabetes = Diabetes,
    alcoholism = Alcoholism,
    handicap = Handcap,
    sms_received = SMS_received,
    no_show = `No-show`
  )


--------------------------------------------------------------------------------
  
#What is the time range of the dataset? ANS: April 20th to June 8th, 2016
appointments %>%
  summarise(
    start_date = min(appointment_day),
    end_date = max(appointment_day)
  )

# Create age groups for easier comparison
appointments <- appointments %>%
  mutate(age_group = case_when(
    age < 1 ~ "Under 1",
    age >= 1 & age <= 4 ~ "1 - 4",
    age >= 5 & age <= 9 ~ "5 - 9",
    age >= 10 & age <= 17 ~ "10 - 17",
    
    age >= 18 & age <= 29 ~ "18 - 29",
    age >= 30 & age <= 39 ~ "30 - 39",
    
    age >= 40 & age <= 49 ~ "40 - 49",
    age >= 50 & age <= 59 ~ "50 - 59",
    
    age >= 60 & age <= 69 ~ "60 - 69",
    age >= 70 & age <= 79 ~ "70 - 79",
    
    age >= 80 ~ "80+"))


# Convert scheduled and appointment days to date formats
appointments <- appointments %>%
  mutate(
    scheduled_day = ymd_hms(scheduled_day),         # Keep time
    appointment_day = ymd(appointment_day)           # Keep only date
  )

# Create new column for wait time in days
appointments <- appointments %>%
  mutate(
    wait_days = as.numeric(difftime(appointment_day, as_date(scheduled_day), units = "days"))
  )

# Convert no-show to binary flag: 1 = No-show, 0 = Showed up
appointments <- appointments %>%
  mutate(
    no_show_flag = ifelse(no_show == "Yes", 1, 0)
  )
--------------------------------------------------------------------------------
#RESEARCH 1: Overall summary tables 
  
library(dplyr)
library(lubridate)

# Ensure month column exists
appointments <- appointments %>%
  mutate(month = month(appointment_day, label = TRUE, abbr = FALSE))

# 1. Summary by Month
monthly_summary <- appointments %>%
  group_by(month) %>%
  summarise(
    total_patients = n_distinct(patient_id),
    total_appointments = n(),
    total_no_shows = sum(no_show_flag),
    no_show_rate = mean(no_show_flag),
    .groups = "drop"
  )

# 2. Overall Summary (no month grouping)
overall_summary <- appointments %>%
  summarise(
    month = "Overall",
    total_patients = n_distinct(patient_id),
    total_appointments = n(),
    total_no_shows = sum(no_show_flag),
    no_show_rate = mean(no_show_flag)
  )

# Calculate % of patients with more than one appointment
repeat_stats <- appointments %>%
  group_by(patient_id) %>%
  summarise(n_appointments = n()) %>%
  summarise(
    total_patients = n(),
    repeat_patients = sum(n_appointments > 1),
    percent_repeat = repeat_patients / total_patients * 100
  )
print(repeat_stats)


# 3. Combine overall + monthly
final_summary <- bind_rows(overall_summary, monthly_summary,repeat_stats)

# View
print(final_summary)

--------------------------------------------------------------------------------

## RESEARCH 2: NO - SHOW ANALYSIS OVER THE MONTHS

library(dplyr)
library(lubridate)

monthly_noshow_summary <- appointments %>%
  mutate(month = month(appointment_day, label = TRUE, abbr = FALSE)) %>%
  group_by(month) %>%
  summarise(
    total_appointments = n(),
    total_no_shows = sum(no_show_flag),
    no_show_rate = mean(no_show_flag),
    .groups = "drop"
  )

library(ggplot2)
library(scales)

ggplot(monthly_noshow_summary, aes(x = month, y = no_show_rate, group = 1)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(size = 3, color = "darkblue") +
  geom_text(
    aes(label = scales::percent(no_show_rate, accuracy = 0.1)),
    vjust = -0.8,
    size = 4
  ) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 0.3)) +
  labs(
    title = "Monthly No-Show Rate",
    x = "Month",
    y = "No-Show Rate (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

------------------------------------------------------------------------------

## RESEARCH 3. No-show Rates by hour, chronic condition and Month
#Step 1: Ensure condition_group is created safely

library(dplyr)


appointments <- appointments %>%
  mutate(
    condition_group = case_when(
      Hypertension == 0 & diabetes == 0 ~ "No Conditions",
      Hypertension == 0 & diabetes == 1 ~ "Diabetes Only",
      Hypertension == 1 & diabetes == 0 ~ "Hypertension Only",
      Hypertension == 1 & diabetes == 1 ~ "Both Conditions"
    )
  )


# Step 2: Extract appointment booking hour and month
appointments <- appointments %>%
  mutate(
    appointment_hour = hour(scheduled_day),  # Since actual appointment time is missing
    month = month(appointment_day, label = TRUE, abbr = FALSE)  # Dynamically create month names
  )

# Step 3: Summarize no-show rate by hour, condition group, and month
hourly_noshow_summary <- appointments %>%
  group_by(month, appointment_hour, condition_group) %>%
  summarise(
    total = n(),
    no_shows = sum(no_show_flag),
    no_show_rate = no_shows / total,
    .groups = "drop"
  )

# Step 4: Plot the result
library(ggplot2)
library(scales)

ggplot(hourly_noshow_summary, aes(x = appointment_hour, y = condition_group, fill = no_show_rate)) +
  geom_tile(color = "white") +
  geom_text(aes(label = scales::percent(round(no_show_rate, 1))), size = 3.5, color = "black") +
  facet_wrap(~ month, ncol = 1) +
  scale_x_continuous(
    breaks = 0:23,
    expand = c(0, 0),
    labels = function(x) sprintf("%02d:00", x)
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_gradient(low = "#e0f3f8", high = "#990000", labels = percent_format()) +
  labs(
    title = "Heatmap of No-Show Rate by Booking Hour and Chronic Condition",
    x = "Hour of Day Appointment Was Scheduled",
    y = "Chronic Condition Group",
    fill = "No-Show Rate"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )
--------------------------------------------------------------------------------

## RESEARCH 4.Monthly Differences in No- show rates across age groups and sexes


monthly_age_sex <- appointments %>%
  group_by(month, age_group, gender) %>%
  summarise(
    total = n(),
    no_shows = sum(no_show_flag),
    no_show_rate = no_shows / total,
    .groups = "drop"
  )


## 2-Column (Gender) x 3-Row (Month) Grid

ggplot(monthly_age_sex, aes(x = age_group, y = no_show_rate, fill = gender)) +
  geom_col(width = 0.8) +
  geom_text(
    aes(label = scales::percent(round(no_show_rate, 2))),
    vjust = -0.3,
    size = 3
  ) +
  facet_grid(rows = vars(month), cols = vars(gender)) +  # ← Layout: Month (row) × Gender (col)
  scale_y_continuous(
    labels = percent_format(),
    limits = c(0, 0.35)
  ) +
  labs(
    title = "Monthly No-Show Rate by Age Group and Gender",
    x = "Age Group",
    y = "No-Show Rate",
    fill = "Gender"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

--------------------------------------------------------------------------------

##RESEARCH 5. Average Scheduling delay by month and chronic condition


avg_wait_month_cond <- appointments %>%
  group_by(month, condition_group) %>%
  summarise(
    avg_wait_days = mean(wait_days, na.rm = TRUE),
    .groups = "drop"
  )

library(ggplot2)

ggplot(avg_wait_month_cond, aes(x = month, y = avg_wait_days, fill = condition_group)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.8) +
  geom_text(
    aes(label = round(avg_wait_days, 1)),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3
  ) +
  labs(
    title = "Average Scheduling Delay by Month and Condition",
    x = "Month",
    y = "Avg Wait Time (Days)",
    fill = "Condition"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 12)
  )

--------------------------------------------------------------------------------

## RESEARCH 6 Average scheduling delay by age, gender, condition and month

# Aggregate: average wait by month, gender,condition and age group

wait_stacked <- appointments %>%
  group_by(month, gender, age_group, condition_group) %>%
  summarise(
    avg_wait_days = mean(wait_days, na.rm = TRUE),
    .groups = "drop"
  )

# Plot - Stacked Bar by condition group, facet by gender (columns) and month (rows)
ggplot(wait_stacked, aes(x = age_group, y = avg_wait_days, fill = condition_group)) +
  geom_col(position = "stack", width = 0.8) +
  facet_grid(rows = vars(month), cols = vars(gender)) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(limits = c(0, 45)) +  # ← Set Y-axis scale from 0 to 45
  labs(
    title = "Stacked Average Scheduling Delay by Age, Gender, and Condition",
    subtitle = "Each bar shows delay stacked by condition group (e.g., Diabetes, Hypertension)",
    x = "Age Group",
    y = "Average Wait Time (Days)",
    fill = "Condition Group"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


------------------------------------------------------------------------------------------
#RESEARCH 7: Which age groups and genders are more likely to miss appointments even after receiving SMS reminders?
  sms_noshow_demo <- appointments %>%
  group_by(age_group, gender, sms_received) %>%
  summarise(
    total = n(),
    no_shows = sum(no_show_flag),
    no_show_rate = no_shows / total,
    .groups = "drop"
  )

# View table
print(sms_noshow_demo)

library(ggplot2)

ggplot(sms_noshow_demo, aes(x = age_group, y = no_show_rate, fill = factor(sms_received))) +
  geom_col(position = "dodge", width = 0.8) +
  facet_wrap(~ gender) +
  scale_fill_manual(values = c("0" = "#a6cee3", "1" = "#1f78b4"),
                    labels = c("No SMS", "SMS Sent"),
                    name = "SMS Reminder") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.5)) +
  geom_text(aes(label = scales::percent(round(no_show_rate, 2))), 
            position = position_dodge(width = 0.8), 
            vjust = -0.3, size = 3) +
  labs(
    title = "No-Show Rate by Age, Gender, and SMS Reminder",
    x = "Age Group",
    y = "No-Show Rate"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

--------------------------------------------------------------------------------
## RESEARCH 8: What are the no-show patterns among patients with multiple appointments over the months?
#Are repeat no showers more likely to miss again , and does this pattern change over time?
  
#Step 0: Create a column month based on the appointment day using lubridate package
  
  library(lubridate)

appointments <- appointments %>%
  mutate(
    month = month(appointment_day, label = TRUE, abbr = FALSE)
  )

  #Step 1: Tag total appointments per patients

  
  appointments <- appointments %>%
  group_by(patient_id) %>%
  mutate(
    total_appointments_patient = n()
  ) %>%
  ungroup()

  appointments <- appointments %>%
    group_by(patient_id) %>%
    mutate(total_appts = n()) %>%
    ungroup() %>%
    mutate(freq_group = case_when(
      total_appts == 1 ~ "1",
      total_appts <= 5 ~ "2–5",
      total_appts > 5 ~ "6+"
    ))
  
  frequency_noshow <- appointments %>%
    group_by(freq_group) %>%
    summarise(
      total = n(),
      no_shows = sum(no_show_flag),
      no_show_rate = no_shows / total,
      .groups = "drop"
    )
  
#Step : visualize

library(ggplot2)
ggplot(frequency_noshow, aes(x = freq_group, y = no_show_rate, fill = freq_group)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = scales::percent(no_show_rate, accuracy = 0.1)), vjust = -0.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "No-Show Rate by Appointment Frequency Group",
    x = "Appointment Volume Group",
    y = "No-Show Rate"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

----------------------------------------------------------------------------------
  
#RESEARCH 9: Temporal Trends- Repeat patient trends across months
  
  # Already added `month` and `total_appts` in your data
  repeat_only <- appointments %>% filter(total_appts > 1)

monthly_repeat_noshow <- repeat_only %>%
  group_by(month) %>%
  summarise(
    total = n(),
    no_shows = sum(no_show_flag),
    no_show_rate = no_shows / total,
    .groups = "drop"
  )

library(ggplot2)
library(scales)

ggplot(monthly_repeat_noshow, aes(x = month, y = no_show_rate, group = 1)) +
  geom_line(color = "darkred", size = 1.2) +
  geom_point(size = 3, color = "black") +
  geom_text(aes(label = scales::percent(no_show_rate, accuracy = 0.1)), 
            vjust = -0.6, size = 4.5) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 0.35)) +
  labs(
    title = "Monthly No-Show Rate for Repeat Patients",
    x = "Month",
    y = "No-Show Rate"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

--------------------------------------------------------------------------------
#RESEARCH 10: Patient Typing- Stacked Bar
  
  # Already created `patient_type`
  monthly_ptype_noshow <- appointments %>%
  group_by(month, freq_group) %>%
  summarise(
    total = n(),
    no_shows = sum(no_show_flag),
    .groups = "drop"
  ) %>%
  group_by(month) %>%
  mutate(percent = no_shows / sum(no_shows),
         label = paste0(round(percent * 100, 1), "%"))

ggplot(monthly_ptype_noshow, aes(x = month, y = percent, fill = freq_group)) +
  geom_col(width = 0.7, position = "stack") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "No-Show Composition by Patient Type",
    x = "Month",
    y = "Percentage of No-Shows",
    fill = "freq_group"
  ) +
  theme_minimal()
--------------------------------------------------------------------------------
  
#RESEARCH 11: Demographic Interaction -Gender and Age
  repeat_demo <- appointments %>% filter(total_appts > 1)

repeat_demo_summary <- repeat_demo %>%
  group_by(age_group, gender) %>%
  summarise(
    total = n(),
    no_shows = sum(no_show_flag),
    no_show_rate = no_shows / total,
    .groups = "drop"
  )
ggplot(repeat_demo_summary, aes(x = age_group, y = no_show_rate, fill = gender)) +
  geom_col(position = "dodge") +
  facet_wrap(~gender) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "No-Show Rate by Age Group and Gender (Repeat Patients)",
    x = "Age Group",
    y = "No-Show Rate",
    fill = "Gender"
  ) +
  theme_minimal()

--------------------------------------------------------------------------------
  
#RESEARCH 12: Intervention Response-SMS IMpact by frequency
  sms_effectiveness <- appointments %>%
  group_by(freq_group, sms_received) %>%
  summarise(
    total = n(),
    no_shows = sum(no_show_flag),
    no_show_rate = no_shows / total,
    .groups = "drop"
  )

sms_effectiveness <- sms_effectiveness %>%
  mutate(sms_received = factor(sms_received, levels = c(0, 1), labels = c("No SMS", "SMS Sent")))

ggplot(sms_effectiveness, aes(x = freq_group, y = no_show_rate, fill = sms_received)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = scales::percent(round(no_show_rate, 2))),
            position = position_dodge(0.9), vjust = -0.3) +
  scale_fill_manual(values = c("No SMS" = "#d9d9d9", "SMS Sent" = "#1f78b4"),
                    name = "SMS Reminder") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "SMS Reminder Effectiveness by Frequency Group",
    x = "Frequency Group",
    y = "No-Show Rate"
  ) +
  theme_minimal()

----------------------------------------------------------------------------------
  
#RESEARCH 13: CONDITION- SPECIFIC BEHAVIOUR- CHRONIC +FREQUENT =HIGHER RISK? 
  
  appointments <- appointments %>%
  mutate(chronic_status = case_when(
    Hypertension == 1 & diabetes == 1 ~ "Both",
    Hypertension == 1 ~ "Hypertension",
    diabetes == 1 ~ "Diabetes",
    TRUE ~ "None"
  ))

condition_behavior <- appointments %>%
  group_by(chronic_status, freq_group) %>%
  summarise(
    total = n(),
    no_shows = sum(no_show_flag),
    no_show_rate = no_shows / total,
    .groups = "drop"
  )

ggplot(condition_behavior, aes(x = freq_group, y = no_show_rate, fill = chronic_status)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = scales::percent(no_show_rate, accuracy = 0.1)), 
            position = position_dodge(0.9), vjust = -0.3) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "No-Show Rate by Condition and Appointment Frequency",
    x = "Patient Type",
    y = "No-Show Rate",
    fill = "Condition"
  ) +
  theme_minimal()
---------------------------------------------------------------------------------
  
#RESEARCH 14: PREDICTIVE MODELING- LOGISTIC REGRESSION
  
  # Clean model-ready data
  appointments <- appointments %>%
  mutate(
    gender = as.factor(gender),
    sms_received = as.factor(sms_received),
    Hypertension = as.factor(Hypertension),
    diabetes = as.factor(diabetes),
    freq_group = as.factor(freq_group)
  )

# Logistic regression with added total appointments
logit_model <- glm(
  no_show_flag ~ age + gender + sms_received + wait_days + Hypertension + diabetes + total_appts,
  data = appointments,
  family = "binomial"
)

summary(logit_model)

-------------------------------------------------------------------------------------------
  
  # Load required package
  library(writexl)

# Combine all relevant data frames
all_outputs <- list(
  "Master_Summary_Table" = final_summary,                                # RQ1
  "Monthly_NoShow_Overall" = monthly_noshow_summary,                     # RQ2
  "NoShow_by_Hour_Condition" = hourly_noshow_summary,                   # RQ3
  "Monthly_NoShow_by_Age_Gender" = monthly_age_sex,                     # RQ4
  "Avg_Wait_by_Month_Condition" = avg_wait_month_cond,                 # RQ5
  "Wait_by_Age_Gender_Condition" = wait_stacked,                        # RQ6
  "SMS_by_Age_Gender" = sms_noshow_demo,                                # RQ7
  "Repeat_Patient_NoShow_by_Month" = monthly_repeat_noshow,            # RQ8/9
  "Frequency_vs_NoShow" = frequency_noshow,                             # RQ10
  "Patient_Typing_by_Month" = monthly_ptype_noshow,                     # RQ11
  "Repeat_Demographic_NoShow" = repeat_demo_summary,                    # RQ12
  "SMS_Effectiveness_by_PatientType" = sms_effectiveness,              # RQ13
  "Chronic_vs_PatientType_NoShow" = condition_behavior)
write_xlsx(
  all_outputs,
  path = "C:/Users/srive/onedrive/Desktop/Medical_Appointments_Analysis.xlsx"
)

