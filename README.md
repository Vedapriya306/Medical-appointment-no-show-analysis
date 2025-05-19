# 📊 Medical Appointments - No-Show Analysis

This project analyzes a real-world dataset from medical appointments in Brazil to explore **factors influencing patient no-shows**. The goal is to uncover patterns in missed appointments and assess the effectiveness of interventions like SMS reminders, with the aim of improving operational efficiency and patient adherence.

---

## 📁 Dataset

- **Source:** [Kaggle - Medical Appointment No Shows](https://www.kaggle.com/datasets/joniarroba/noshowappointments)
- **Time Range:** April 20 – June 8, 2016
- **Observations:** 110,527 appointments
- **Key Features:**
  - Demographics: age, gender, neighborhood
  - Appointment details: scheduled date, appointment date, wait time
  - Health indicators: hypertension, diabetes, alcoholism, handicap
  - Interventions: SMS reminders
  - Target: No-show status (`No-show`)

---

## 🔍 Research Questions

This analysis addresses **14 core questions**, grouped by theme:

### 📅 Temporal Patterns
1. **What is the overall no-show rate and monthly trends?**
2. **How does no-show behavior vary month-to-month?**
3. **How do no-show rates vary by appointment hour and chronic conditions?**

### 👥 Demographics & Social Factors
4. **Are there differences in no-show rates across age groups and genders by month?**
5. **What is the average scheduling delay across months and conditions?**
6. **How does wait time vary by age, gender, and condition group?**

### 🔔 Intervention Effectiveness
7. **Which age and gender groups are more likely to miss appointments despite receiving SMS reminders?**
8. **Do patients with multiple appointments show a consistent no-show pattern over time?**
9. **What are the monthly no-show trends among repeat patients?**

### 🔄 Patient Segmentation
10. **What proportion of no-shows come from different patient frequency types (e.g., single vs. frequent visitors)?**
11. **How do age and gender interact with visit frequency to influence no-show risk?**
12. **Is the effectiveness of SMS reminders different across visit frequency groups?**

### 🏥 Health Condition-Specific
13. **Do patients with chronic conditions (hypertension/diabetes) and high appointment frequency show increased no-show rates?**

### 🤖 Predictive Modeling
14. **Can we build a logistic regression model to predict no-shows based on age, gender, wait time, chronic conditions, SMS reminders, and appointment history?**

---

## 📦 Key Features of the Analysis

- `tidyverse` and `lubridate` used for data wrangling
- `ggplot2` used for high-quality data visualizations
- `writexl` used to export all summary tables into an Excel workbook
- Logistic regression implemented to predict no-show risk


