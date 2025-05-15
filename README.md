# Medical Appointments No-Show Analysis

This project analyzes patient no-show patterns using a public dataset of medical appointments in Brazil. The goal is to explore how demographic, scheduling, and clinical factors affect whether patients attend their scheduled appointments.

## 📊 Dataset

- **Source**: [Kaggle - Medical Appointment No Shows](https://www.kaggle.com/datasets/joniarroba/noshowappointments)
- **Rows**: 110,000+ appointment records
- **Key Features**:
  - `Gender`, `Age`, `Scholarship` (yes/no)
  - `Hypertension`, `Diabetes`, `Alcoholism`, `Handicap`
  - `ScheduledDay`, `AppointmentDay`
  - `SMS_received`, `No-show`

## 🧪 Research Questions

1. **Time Behavior**:  
   - How do no-show rates vary by appointment booking hour and chronic condition?

2. **Demographic Disparities**:  
   - Are there monthly differences in no-show rates across age groups and sexes?

3. **Scheduling Delay**:  
   - What is the average time taken to schedule an appointment?
   - Does it vary by month and patient condition?

4. **Equity in Access**:  
   - Are specific demographic-condition combinations experiencing longer wait times?

## 📈 Visualizations

The analysis includes:
- Heatmaps of no-show rate by hour and condition
- Faceted bar plots by gender, age group, and month
- Stacked charts showing average wait time by chronic condition
- Summary tables exported to Excel



