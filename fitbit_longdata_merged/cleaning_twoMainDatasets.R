library(readr)
library(skimr)
library(dplyr)
library(lubridate)
options(scipen = 999)

#This scripts is to generate user_category and dropped version of two datasets (dailyData & dailyActivity)

#dailyData_dataset.csv
df <- read_csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/fitbit_longdata_merged/cleaned/dailyData_dataset_v02.csv')
View(df)
str(df)

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

df_clean <- df_clean %>%
  mutate(date = as.Date(date, format = "%m-%d-%Y"))


write.csv(df_clean, "/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/fitbit_longdata_merged/cleaned/dailyData_dataset_v03.csv", row.names = FALSE)

# Categorizing users from the distribution of their frequency using the device, distributed by Quintile
q1 <- quantile(count_df$date_count, 0.28) # 25th percentile
q2 <- quantile(count_df$date_count, 0.48) # 50th percentile (median)
q3 <- quantile(count_df$date_count, 0.75) # 75th percentile

# Define categories
count_df$user_label <- cut(count_df$date_count, 
                         breaks = c(-Inf, q1, q2, q3, Inf), 
                         labels = c("Light User", "Moderate User", "Frequent User", "Heavy User"),
                         include.lowest = TRUE)
View(count_df)

# View categorized data
table(count_df$user_label)

ggplot(count_df, aes(x = date_count, fill = user_label )) +
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



#dailyActivity_dataset.csv
df_activity <- read_csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/fitbit_longdata_merged/cleaned/dailyActivity_dataset_V01.csv')


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

df_activity <- df_activity %>%
  mutate(date = as.Date(date, format = "%m-%d-%Y"))

write.csv(df_activity, "/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/fitbit_longdata_merged/cleaned/dailyActivity_dataset_v02.csv", row.names = FALSE)

