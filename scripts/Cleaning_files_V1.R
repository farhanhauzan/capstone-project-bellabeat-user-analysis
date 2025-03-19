library(dplyr)
library(tidyr)
library(lubridate)

df_dailyActivity1 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/archive/mturkfitbit_export_3.12.16-4.11.16/dailyActivity_merged.csv')
df_dailyActivity2 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/archive/mturkfitbit_export_4.12.16-5.12.16/dailyActivity_merged.csv')
df_da <- bind_rows(df_dailyActivity1, df_dailyActivity2)
write.csv(df_da, "dailyActivity_merged.csv", row.names = FALSE)

df_heart1 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/archive/mturkfitbit_export_3.12.16-4.11.16/heartrate_seconds_merged.csv')
df_heart2 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/archive/mturkfitbit_export_4.12.16-5.12.16/heartrate_seconds_merged.csv')
df_heart <- bind_rows(df_heart1, df_heart2)
write.csv(df_heart, "heartrate_seconds_merged.csv", row.names = FALSE)

df_hcm1 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/archive/mturkfitbit_export_3.12.16-4.11.16/hourlyCalories_merged.csv')
df_hcm2 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/archive/mturkfitbit_export_4.12.16-5.12.16/hourlyCalories_merged.csv')
df_calories <- bind_rows(df_hcm1, df_hcm2)
write.csv(df_calories, "hourlyCalories_merged.csv", row.names = FALSE)

df_him1 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/mturkfitbit_export_3.12.16-4.11.16/hourlyIntensities_merged.csv')
df_him2 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/mturkfitbit_export_4.12.16-5.12.16/hourlyIntensities_merged.csv')
df_intensity <- bind_rows(df_him1, df_him2)
write.csv(df_him, "hourlyIntensities_merged.csv", row.names = FALSE)

df_steps1 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/archive/mturkfitbit_export_3.12.16-4.11.16/hourlySteps_merged.csv')
df_steps2 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/archive/mturkfitbit_export_4.12.16-5.12.16/hourlySteps_merged.csv')
df_step <- bind_rows(df_steps1, df_steps2)
write.csv(df_steps, "hourlySteps_merged.csv", row.names = FALSE)

df_mcn1 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/archive/mturkfitbit_export_3.12.16-4.11.16/minuteCaloriesNarrow_merged.csv')
df_mcn2 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/archive/mturkfitbit_export_4.12.16-5.12.16/minuteCaloriesNarrow_merged.csv')
df_mcn <- bind_rows(df_mcn1, df_mcn2)
write.csv(df_mcn, "minuteCaloriesNarrow_merged.csv", row.names = FALSE)

df_min1 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/archive/mturkfitbit_export_3.12.16-4.11.16/minuteIntensitiesNarrow_merged.csv')
df_min2 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/archive/mturkfitbit_export_4.12.16-5.12.16/minuteIntensitiesNarrow_merged.csv')
df_min <- bind_rows(df_min1, df_min2)
write.csv(df_min, "minuteIntensitiesNarrow_merged.csv", row.names = FALSE)

df_met1 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/archive/mturkfitbit_export_3.12.16-4.11.16/minuteMETsNarrow_merged.csv')
df_met2 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/archive/mturkfitbit_export_4.12.16-5.12.16/minuteMETsNarrow_merged.csv')
df_mets <- bind_rows(df_met1, df_met2)
write.csv(df_mets, "minuteMETsNarrow_merged.csv", row.names = FALSE)

df_slp1 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/archive/mturkfitbit_export_3.12.16-4.11.16/minuteSleep_merged.csv')
df_slp2 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/archive/mturkfitbit_export_4.12.16-5.12.16/minuteSleep_merged.csv')
df_sleep <- bind_rows(df_slp1, df_slp2)
write.csv(df_sleep, "minuteSleep_merged.csv", row.names = FALSE)

df_stn1 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/archive/mturkfitbit_export_3.12.16-4.11.16/minuteStepsNarrow_merged.csv')
df_stn2 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/archive/mturkfitbit_export_4.12.16-5.12.16/minuteStepsNarrow_merged.csv')
df_stn <- bind_rows(df_stn1, df_stn2)
write.csv(df_stn, "minuteSleep_merged.csv", row.names = FALSE)

df_weigh1 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/archive/mturkfitbit_export_3.12.16-4.11.16/weightLogInfo_merged.csv')
df_weigh2 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/archive/mturkfitbit_export_4.12.16-5.12.16/weightLogInfo_merged.csv')
df_weigh <- bind_rows(df_weigh1, df_weigh2)
write.csv(df_weigh, "weightLogInfo_merged.csv", row.names = FALSE)

# so we are going to make analysis based on 4 tables, Sleep, Steps, Intensity and METs to measure the user profile

n_distinct(df_da$Id) #35
n_distinct(df_intensity$Id) #35
n_distinct(df_step$Id) #35
n_distinct(df_sleep$Id) #25

str(df_da) #Datecolumn data type needs to be change to Date
str(df_intensity) #Date&time located in the same column we think it is better to separate into different column
str(df_step) #Datecolumn data type needs to be change to Date, also separate into different column
str(df_sleep) #Datecolumn data type needs to be change to Date
View(df_step) #LogId probably useless for data analysis

# Data Cleaning

  # Removed unnecessary records.
  # Removed trailing spaces from all the Queries.
  # Removed records from date 3.11.2016 from the Sleep file – date not in the observation timeframe.
  # Inconsistencies in the intensity_min data. For a few days the total amount of time sums up to more than 1440 min (a day). Removed dates with inconsistencies: 4.5.2016, 4.6.2016, 4.11.2016, 4.12.2016, 5.6.2016, 5.7.2016.
  # Removed IDs not part of the sample.
  # Changed the mismatched data types.
  # Split date-time columns into two separate columns: date, time.
  # Divided METs by 10 to get accurate values.
  # Changed the dates format consistently throughout the dataset.
  # Renamed the columns clear and consistent

#Cleaning df_sleep using lubridate to separate date time
View(df_sleep)
datetime_parsed <- mdy_hms(df_sleep $date) #changing string to usable datetime DT

df_sleep$date <- as.Date(datetime_parsed)
df_sleep$time <- format(datetime_parsed, "%I:%M:%S %p")
df_sleep$date <- format(df_sleep$date, "%m-%d-%Y")

df_sleep <- df_sleep %>%
  select(-ActivityHour) %>%  # Drop ActivityHour  
  rename(user_id = Id) %>%  # Rename Id to user_id  
  select(user_id, date, time, value) %>%  # Keep only required columns  
  arrange(user_id, date, time)  # Sort by user_id, date, and time  

df_sleep$date <- format(df_sleep$date, "%m-%d-%Y")

View(df_sleep)
write.csv(df_sleep, "/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/fitbit_longdata_merged/cleaned/minuteSleep_data.csv", row.names = FALSE) #export to finalized cleaning


#Cleaning df_step using lubridate to separate date time

# Separate date and time
datetime_parsed <- mdy_hms(df_step$ActivityHour) #changing string to usable datetime DT

df_step$date <- as.Date(datetime_parsed)
df_step$time <- format(datetime_parsed, "%I:%M:%S %p")

write.csv(df_step, "hourlySteps_cleaned.csv", row.names = FALSE) #export to finalized cleaning

df_step <- df_step %>%
  select(-ActivityHour) %>%  # Drop ActivityHour  
  rename(user_id = Id, totalStep = StepTotal) %>%  # Rename columns  
  select(user_id, date, time, totalStep)  # Keep only required columns  

df_step$date <- format(df_step$date, "%m-%d-%Y")

df_step$totalStep[df_step$totalStep == 0] <- "" #change 0 to NA to standardize
df_step <- rename(df_step, total_step = tota_step)

View(df_step)  # Display the final dataframe  
write.csv(df_step, "/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/fitbit_longdata_merged/cleaned/dailySteps_data.csv", row.names = FALSE, quote = TRUE)


#Cleaning df_calories using lubridate to separate date time

View(df_calories)
datetime_parsed <- mdy_hms(df_calories $ActivityHour) #changing string to usable datetime DT

df_calories$date <- as.Date(datetime_parsed)
df_calories$time <- format(datetime_parsed, "%I:%M:%S %p")
df_calories$date <- format(df_calories$date, "%m-%d-%Y")

df_calories <- df_calories %>%
  select(-ActivityHour) %>%  # Drop ActivityHour  
  rename(user_id = Id, caloriesExpenditure = Calories) %>%  # Rename columns  
  select(user_id, date, time, caloriesExpenditure) %>%  # Keep only required columns  
  arrange(user_id, date)  # Sort by user_id first, then date  

df_calories$date <- format(df_calories$date, "%m-%d-%Y")

View(df_calories)
write.csv(df_calories, "hourlyCalories_data.csv", row.names = FALSE) #export to finalized cleaning

#Cleaning df_METs using lubridate to separate date time

datetime_parsed <- mdy_hms(df_mets $ActivityMinute) #changing string to usable datetime DT

df_mets$date <- as.Date(datetime_parsed)
df_mets$time <- format(datetime_parsed, "%I:%M:%S %p")
df_mets$date <- format(df_mets$date, "%m-%d-%Y")

df_mets <- df_mets %>%
  select(-ActivityMinute) %>%  # Drop ActivityMinute  
  rename(user_id = Id, energyExpenditure = METs) %>%  # Rename columns  
  select(user_id, date, time, energyExpenditure) %>%  # Keep only required columns  
  arrange(user_id, date)  # Sort by user_id first, then date  

View(df_mets)
write.csv(df_mets, "/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/fitbit_longdata_merged/cleaned/minuteMETs_data.csv", row.names = FALSE, quote = TRUE) #export to finalized cleaning


#Cleaning df_Intensities using lubridate to separate date time

View(df_intensity)

datetime_parsed <- mdy_hms(df_intensity $ActivityHour) #changing string to usable datetime DT

df_intensity$date <- as.Date(datetime_parsed)
df_intensity$time <- format(datetime_parsed, "%I:%M:%S %p")
df_intensity$date <- format(df_intensity$date, "%m-%d-%Y")

df_intensity <- select(df_intensity, -ActivityHour)#drop ActivityMinute

df_intensity <- df_intensity %>%
  rename(user_id = Id, 
         total_intensity = TotalIntensity, 
         average_intensity = AverageIntensity) %>%
  select(user_id, date, time, total_intensity, average_intensity) %>%
  arrange(user_id, date)  # Sort by user_id first, then date

df_intensity <- df_intensity %>%
  mutate(across(c(average_intensity, total_intensity), ~ ifelse(. == 0, "", .))) #change 0 to NA to standardize

View(df_intensity)
write.csv(df_intensity, "/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/fitbit_longdata_merged/cleaned/hourlyIntensity_data.csv", row.names = FALSE, quote = TRUE) #export to finalized cleaning

