setwd('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/data/raw/fitbit_longdata_merged')
getwd()
library(dplyr)

df_dailyActivity1 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/data/raw/mturkfitbit_export_3.12.16-4.11.16/dailyActivity_merged.csv')
df_dailyActivity2 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/data/raw/mturkfitbit_export_4.12.16-5.12.16/dailyActivity_merged.csv')
df_da <- bind_rows(df_dailyActivity1, df_dailyActivity2)
write.csv(df_da, "dailyActivity_merged.csv", row.names = FALSE)

df_heart1 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/data/raw/mturkfitbit_export_3.12.16-4.11.16/heartrate_seconds_merged.csv')
df_heart2 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/data/raw/mturkfitbit_export_4.12.16-5.12.16/heartrate_seconds_merged.csv')
df_heart <- bind_rows(df_heart1, df_heart2)
write.csv(df_heart, "heartrate_seconds_merged.csv", row.names = FALSE)

df_hcm1 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/data/raw/mturkfitbit_export_3.12.16-4.11.16/hourlyCalories_merged.csv')
df_hcm2 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/data/raw/mturkfitbit_export_4.12.16-5.12.16/hourlyCalories_merged.csv')
df_hcm <- bind_rows(df_hcm1, df_hcm2)
write.csv(df_hcm, "hourlyCalories_merged.csv", row.names = FALSE)

df_him1 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/data/raw/mturkfitbit_export_3.12.16-4.11.16/hourlyIntensities_merged.csv')
df_him2 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/data/raw/mturkfitbit_export_4.12.16-5.12.16/hourlyIntensities_merged.csv')
df_him <- bind_rows(df_him1, df_him2)
write.csv(df_him, "hourlyIntensities_merged.csv", row.names = FALSE)

df_steps1 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/data/raw/mturkfitbit_export_3.12.16-4.11.16/hourlySteps_merged.csv')
df_steps2 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/data/raw/mturkfitbit_export_4.12.16-5.12.16/hourlySteps_merged.csv')
df_steps <- bind_rows(df_steps1, df_steps2)
write.csv(df_steps, "hourlySteps_merged.csv", row.names = FALSE)

df_mcn1 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/data/raw/mturkfitbit_export_3.12.16-4.11.16/minuteCaloriesNarrow_merged.csv')
df_mcn2 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/data/raw/mturkfitbit_export_4.12.16-5.12.16/minuteCaloriesNarrow_merged.csv')
df_mcn <- bind_rows(df_mcn1, df_mcn2)
write.csv(df_mcn, "minuteCaloriesNarrow_merged.csv", row.names = FALSE)

df_min1 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/data/raw/mturkfitbit_export_3.12.16-4.11.16/minuteIntensitiesNarrow_merged.csv')
df_min2 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/data/raw/mturkfitbit_export_4.12.16-5.12.16/minuteIntensitiesNarrow_merged.csv')
df_min <- bind_rows(df_min1, df_min2)
write.csv(df_min, "minuteIntensitiesNarrow_merged.csv", row.names = FALSE)

df_met1 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/data/raw/mturkfitbit_export_3.12.16-4.11.16/minuteMETsNarrow_merged.csv')
df_met2 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/data/raw/mturkfitbit_export_4.12.16-5.12.16/minuteMETsNarrow_merged.csv')
df_met <- bind_rows(df_met1, df_met2)
write.csv(df_met, "minuteMETsNarrow_merged.csv", row.names = FALSE)

df_slp1 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/data/raw/mturkfitbit_export_3.12.16-4.11.16/minuteSleep_merged.csv')
df_slp2 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/data/raw/mturkfitbit_export_4.12.16-5.12.16/minuteSleep_merged.csv')
df_slp <- bind_rows(df_slp1, df_slp2)
write.csv(df_slp, "minuteSleep_merged.csv", row.names = FALSE)

df_stn1 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/data/raw/mturkfitbit_export_3.12.16-4.11.16/minuteStepsNarrow_merged.csv')
df_stn2 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/data/raw/mturkfitbit_export_4.12.16-5.12.16/minuteStepsNarrow_merged.csv')
df_stn <- bind_rows(df_stn1, df_stn2)
write.csv(df_stn, "minuteSleep_merged.csv", row.names = FALSE)

df_weigh1 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/data/raw/mturkfitbit_export_3.12.16-4.11.16/weightLogInfo_merged.csv')
df_weigh2 <- read.csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/data/raw/mturkfitbit_export_4.12.16-5.12.16/weightLogInfo_merged.csv')
df_weigh <- bind_rows(df_weigh1, df_weigh2)
write.csv(df_weigh, "weightLogInfo_merged.csv", row.names = FALSE)
