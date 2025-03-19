#importing packages
library(tidyverse)
library(lubridate)

#reading file dailyCalories
df_calories <- read.csv("/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/fitbit_longdata_merged/cleaned/dailyCalories_data.csv")
View(df_calories)
str(df_calories) # R read date as char

full_dates <- seq(as.Date("2016-03-12"), as.Date("2016-05-12"), by = "day")

# Get all unique user_ids
user_ids <- unique(df_calories$user_id)

# Create a complete dataframe with all user_id-date combinations
df_full <- expand.grid(user_id = user_ids, date = full_dates)

# Left join to merge with the existing data
df_complete <- df_full %>%
  left_join(df_calories, by = c("user_id", "date"))

# Replace NA values with empty string
df_complete[is.na(df_complete)] <- ""

df_complete$date <- format(as.Date(df_complete$date), "%m-%d-%Y")


View(df_complete)
setwd('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/fitbit_longdata_merged/cleaned')

# Write to a CSV file
write.csv(df_complete, "/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/fitbit_longdata_merged/cleaned/dailyCalories_data_v02.csv", row.names = FALSE, na='')
