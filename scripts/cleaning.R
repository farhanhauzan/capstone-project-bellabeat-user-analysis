library(readr)
library(skimr)
library(dplyr)
library(lubridate)
options(scipen = 999)

#This scripts is to generate user_category and dropped version of two datasets (dailyData & dailyActivity)
#This scripts is to clean mets datasets

#dailyData_dataset.csv
df <- read_csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/data/raw/dailyData_merged.csv')

#Cleaning

#dropping rows of certain character
df_clean <- df %>% filter(average_metabolicEquivalent > 1) #removing inactive or low-activity records METs value below 1

#checking numbers of count date for every user
count_df <- df_clean %>%
  group_by(user_id) %>%
  summarize(date_count = n_distinct(date)) %>%
  arrange(date_count)

print(count_df)

#dropping rows that has less than 20 date_count because a user only has 16 records
df_clean <- df_clean %>%
  group_by(user_id) %>% 
  filter(n_distinct(date) >= 20) %>%
  ungroup()

#re-checking numbers of count date for every user
count_df <- df_clean %>%
  group_by(user_id) %>%
  summarize(date_count = n_distinct(date)) %>%
  arrange(date_count)

View(count_df)
View(df_clean)


#Manipulation

# User Profiling

#Adding sleeping_range column based on their avg time of sleep
df_y <- df_clean %>%
  group_by(user_id) %>%
  summarise(mean_value = mean(sleep_mins, na.rm = TRUE)) %>%
  filter(!is.nan(mean_value)) %>% 
  mutate(sleep_range = case_when(
    mean_value < 360 ~ "Less than 6h",
    mean_value >= 360 & mean_value <= 540 ~ "6-9h of sleep",
    mean_value > 540 ~ "More than 9h"
  ))

#Joining Sleeping range data to initial dataset
df_clean <- df_clean %>%
  left_join(df_y %>% select(user_id, sleep_range), by = "user_id")

View(df_clean)
#Adding user_label by usage frequency

#Adding date_count as identifier the frequency of user wearing device
df_clean$date_count <- ave(match(df_clean$date, df_clean$date), df_clean$user_id, FUN = function(x) length(unique(x)))
View(df_clean)

q1 <- quantile(df_clean$date_count, 0.25) # 25th percentile
q2 <- quantile(df_clean$date_count, 0.50) # 50th percentile (median)
q3 <- quantile(df_clean$date_count, 0.75) # 75th percentile

# Define categories
df_clean$user_label <- cut(df_clean$date_count, 
                          breaks = c(-Inf, q1, q2, q3, Inf), 
                          labels = c("Light User", "Moderate User", "Frequent User", "Heavy User"),
                          include.lowest = TRUE,
                          right = FALSE)

# View categorized data
table(count_df$user_label)

ggplot(df_clean, aes(x = date_count, fill = user_label )) +
  geom_histogram(bins = 40) +
  scale_fill_manual(values = c(    "Light User" = "#F0DF64",
                                   "Moderate User" = "#CCE8CC",
                                   "Frequent User" = "#E78F8E",
                                   "Heavy User" = "#F78D74" )) +
  labs(title = "Distribution of User Device Usage Frequency",
       x = "Device Usage Frequency (date_count)",
       y = "Number of Users") +
  theme_minimal()

View(count_df)

write.csv(df_clean, "/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/data/clean/dailyData_dataset.csv", row.names = FALSE)


#dailyActivity_dataset.csv
df_activity <- read_csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/data/raw/dailyActivity_merged.csv')


#checking numbers of count date for every user
c_df <- df_activity %>%
  group_by(user_id) %>%
  summarize(date_count = n_distinct(date)) %>%
  arrange(date_count)

#dropping rows that has less than 10 date_count because a user only has 16 records
c_df <- c_df %>% filter(date_count > 10)

#re-checking numbers of count date for every user
c_df <- df_activity %>%
  group_by(user_id) %>%
  summarize(date_count = n_distinct(date)) %>%
  arrange(date_count)


#joining table of df activity with labeled user table

df_data_act <- df_data %>%
  distinct(user_id, .keep_all = TRUE)

df_activity <- df_act1 %>%
  left_join(df_data_act %>% select(user_id, user_label), by = "user_id")

#Adding weekday column to the act df
df_activity <- df_activity %>% 
  mutate(weekday = weekdays(date))

df_activity$weekday <-ordered(df_activity$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday",
                                                  "Friday", "Saturday", "Sunday"))

write.csv(df_activity, '/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/data/clean/dailyActivity_dataset.csv', row.names = FALSE)


#hourlyMet_dataset.csv

df_met <- read_csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/data/raw/hourlyMETs_merged.csv')
View(df_met)

#Joining table from label user to hourly Met table
df_data_label <- df_data %>%
  group_by(user_id) %>%
  summarize(user_label = paste(unique(user_label), collapse = ", "))

df_data_label

df_met <- df_met %>%
  left_join(df_data_label %>% select(user_id, user_label), by = "user_id")

df_met <- df_met %>% 
  mutate(weekday = weekdays(date))

df_met$weekday <-ordered(df_met$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday",
                                                            "Friday", "Saturday", "Sunday"))

View(df_met)
str(df_met)

write.csv(df_met, "/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/data/clean/hourlyMETs_data.csv", row.names = FALSE)
