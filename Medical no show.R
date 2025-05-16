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

# 3. Combine overall + monthly
final_summary <- bind_rows(overall_summary, monthly_summary)

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


--------------------------------------------------------------------------------


# Load required package
library(writexl)

# Combine all summary data frames into one list
all_outputs <- list(
  "NoShow_by_Hour_Condition" = hourly_noshow_summary,
  "Monthly_NoShow_by_Age_Gender" = monthly_age_sex,
  "Avg_Wait_by_Month_Condition" = avg_wait_month_cond,
  "Wait_by_Age_Gender_Condition" = wait_stacked,
  "Master_Summary_Table" = final_summary
)

write_xlsx(
  all_outputs,
  path = "C:/Users/srive/onedrive/Desktop/Medical_Appointments_Analysis.xlsx"
)







