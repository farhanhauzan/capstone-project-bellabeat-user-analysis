library(dplyr)
library(skimr)
library(data.table)
library(ggplot2)
library(patchwork)
library(productplots)
library(ggalluvial)
library(ggmosaic)

options(scipen = 999)

#Importing two of the datasets

#User data dateset
df_data <- read_csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/data/clean/dailyData_dataset.csv')

#Skimming and reducing decimals while skimming
skim_with(numeric = sfl(mean = ~round(.x, 2), sd = ~round(.x, 2)))
View(df_data%>% skim() %>% 
       mutate(across(where(is.numeric), ~ round(.x, 2))))#skimming with lower numbers of decimals
df_data %>%
  summarise(unique_users = n_distinct(user_id))

View(df_data)

#User activity dataset

df_act <- read_csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/data/clean/dailyActivity_dataset.csv')
#Skimming and reducing decimals while skimming
skim_with(numeric = sfl(mean = ~round(.x, 2), sd = ~round(.x, 2)))
View(df_act %>% skim() %>% 
       mutate(across(where(is.numeric), ~ round(.x, 2))))#skimming with lower numbers of decimals
df_act %>%
  summarise(unique_users = n_distinct(user_id))

View(df_act)

#User hourly METs

df_met <- read_csv('/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/data/clean/hourlyMETs_data.csv')
View(df_met)


# Creating table of Count distinct sleep users whose all records are has value sleep records

count_users_has_sleep_records<- function(df, column_name) {
  df %>%
    group_by(user_id) %>%
    summarise(any_not_na = any(!is.na(.data[[column_name]])), .groups = "drop") %>%
    filter(any_not_na) %>%
    nrow()
}

count_filled_and_na_rows <- function(df, column_name) {
  df %>%
    summarise(
      filled_rows = sum(!is.na(.data[[column_name]])),  # Count non-NA values
      na_rows = sum(is.na(.data[[column_name]]))        # Count NA values
    )
}

count_users_has_sleep_records(df_data, "sleep_mins")

df_sleep_filled<-count_filled_and_na_rows(df_data, "sleep_mins")

df_sleep_filled <- df_sleep_filled %>%
  pivot_longer(cols = everything(), names_to = "RowType", values_to = "Count")

df_sleep_filled <- df_sleep_filled %>%
  mutate(records_percentage = round((Count / sum(Count)) * 100, 1))

View(df_sleep_filled)

# Generating average of sleep, step, and calories user data by Weekday

df_date_today <- df_data %>% 
  mutate(weekday = weekdays(date))


df_date_today <- df_date_today%>%
  group_by(weekday) %>%
  summarize (daily_steps = mean(total_steps, na.rm = TRUE), daily_sleep = mean(sleep_mins, na.rm = TRUE), daily_calories = mean(calories_expenditure, na.rm = TRUE))

df_date_today$weekday <-ordered(df_date_today$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday",
                                                                "Friday", "Saturday", "Sunday"))

df_ordered <- df_date_today[order(df_date_today$weekday), ] 

View(df_ordered)

#Generate total_active_time from other active category column
df_act <- df_act %>%
  mutate(total_active_time = veryActive_minutes + fairlyActive_minutes + lightlyActive_minutes)
View(df_act)

# Generate summary dataframe
df_summary <- df_act %>%
  filter(!is.na(user_label)) %>% 
  group_by(user_label) %>%
  summarise(
    avg_total_steps = mean(total_steps, na.rm = TRUE),
    avg_total_active_time = mean(total_active_time, na.rm = TRUE),
    avg_sedentary_minutes = mean(sedentaryActive_minutes, na.rm = TRUE),
    avg_total_distance = mean(total_distance),
    .groups = "drop"  # Ungroup after summarizing
  )
View(df_summary)

#Generating column to see each user_label of their average distance, Sedentary time, Active time and, Average steps.

View(df_act)

# Convert wide format to long format
df_long <- df_act %>%
  select(user_id, veryActive_minutes, fairlyActive_minutes, lightlyActive_minutes, sedentaryActive_minutes) %>%
  pivot_longer(cols = -user_id, names_to = "ActivityType", values_to = "Minutes")

df_long <- df_long %>% 
  group_by(ActivityType) %>% 
  summarise(Minutes = mean(Minutes))

unique(df_long$ActivityType)
# Create a ActivityType# Create a horizontal bar chart
ggplot(df_long, aes(x = Minutes, y = reorder(ActivityType, Minutes), fill=factor(ifelse(ActivityType=="sedentaryActive_minutes","Highlighted","Normal"))  # White for all others
)) +
  geom_col(width= 0.5) +  # Use geom_col() since we already have y-values
  scale_fill_manual(name = "area", values=c("#F78D74","grey90")) +
  geom_text(aes(label = round(Minutes, 1)),  
            hjust = -0.4,  # Move text inside the bars
            color = "black",  # Adjust color
            size = 3) +  # Adjust text size
  labs(
    title = "User average activity types (minutes)",
    x = "Minutes",
    y = "Activity Type",
    fill = "Activity Level"
  ) +
  xlim(0, max(df_long$Minutes) * 1.1)+
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "none"  # Hide legend if not needed
  )

ggsave("/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/images/user_average_activity_time.png", width = 10, height = 5, dpi = 300, bg = "white")


# Visualization of average sleep, steps and calories

avg_sleep_plot <- ggplot(df_ordered, aes(x = weekday, y = daily_sleep, fill = daily_sleep)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.7) +  # Thicker bars
  labs(title = "Weekly average Daily Sleep ",
       x = "Weekday",
       y = "Average Sleep (minutes)") +
  scale_y_continuous(limits = c(0, max(df_ordered$daily_sleep) * 1.3),  # More space above bars
                     expand = expansion(mult = c(0, 0.05))) +  # Prevent bars from touching the top
  scale_fill_gradient(low = "#FEFADC", high = "#cce8cc")+
  theme_minimal() +
  theme(plot.title = element_text(size = 10, hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),  # Rotate x-axis labels
        axis.text.y = element_text(size = 10),
        panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"))  # Add reference gridlines

avg_step_plot <- ggplot(df_ordered, aes(x = weekday, y = daily_steps, fill = daily_steps)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.7) +  # Thicker bars
  labs(title = "Weekly average Daily Steps",
       x = "Weekday",
       y = "Average Steps") +
  scale_y_continuous(limits = c(0, max(df_ordered$daily_steps) * 1.3),  # More space above bars
                     expand = expansion(mult = c(0, 0.05))) +  # Prevent bars from touching the top
  scale_fill_gradient(low = "#957A79", high = "#935E52")+
  theme_minimal() +
  theme(plot.title = element_text(size = 10, hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),  # Rotate x-axis labels
        axis.text.y = element_text(size = 10),
        panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"))  # Add reference gridlines

avg_calorie_plot <- ggplot(df_ordered, aes(x = weekday, y = daily_calories, fill = daily_calories)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.7) +  # Thicker bars
  labs(title = "Weekly average Calories",
       x = "Weekday",
       y = "Average Calories (kcal)") +
  scale_y_continuous(limits = c(0, max(df_ordered$daily_calories) * 1.3),  # More space above bars
                     expand = expansion(mult = c(0, 0.05))) +  # Prevent bars from touching the top
  scale_fill_gradient(low = "#FAC4C3", high = "#F78D74")+
  theme_minimal() +
  theme(plot.title = element_text(size = 10, hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),  # Rotate x-axis labels
        axis.text.y = element_text(size = 10),
        panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"))  # Add reference gridlines

avg_sleep_plot + avg_step_plot + avg_calorie_plot 

ggsave("/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/images/weekly_distribution_sleepStepCalories.png", width = 8, height = 5, dpi = 300, bg = "white")

# HEATMAP METS

# Areachart plots
ggplot(df_avg, aes(x = time, y = Average_METs, fill = Weekday)) +
  geom_area(alpha = 0.3, color = "#f78d74", fill = "#f78d74") +  
  facet_wrap(~Weekday, scales = "free_x") +  
  theme_minimal() +
  labs(title = "Users average METs by weekday and time", 
       x = "Time of Day", 
       y = "Avg METs") + 
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,), plot.title = element_text(face = "bold", size = 14, hjust = 0.5),)

ggsave("/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/images/weekly_distribution_Mets.png", width = 8, height = 5, dpi = 300, bg = "white")

View(df_data)

# User Profiling

#Creating Mosaic for type of user distributions

df_mosaic <- df_data %>%
  group_by(user_label) %>%
  summarise(user_count = n_distinct(user_id),
            min_date = min(date_count),
            max_date = max(date_count), .groups = "drop")

# Create a label with both user count & range of date_count
df_mosaic$label <- paste0(df_mosaic$user_label, "\n", 
                          "Users: ", df_mosaic$user_count, "\n", 
                          "Usage: ", df_mosaic$min_date, " - ", df_mosaic$max_date, " days")

# Plot Treemap
ggplot(df_mosaic, aes(area = user_count, fill = user_label, label = label)) +
  geom_treemap() +
  geom_treemap_text(colour = "#2F2F2F", place = "centre", grow = FALSE, size = 12) +
  scale_fill_manual(values = c(
    "Light User" = "#FEFADC",   # Light Blue
    "Moderate User" = "#CCE8CC", # Sky Blue
    "Frequent User" = "#E78F8E", # Steel Blue
    "Heavy User" = "#F78D74"     # Dodger Blue"2f2f2f"
  )) +
  labs(title = "User Label Distribution Based on Usage Days") +
  theme_minimal()+
  theme(plot.title = element_text(face = "bold", size = 14), legend.position = "none")

ggsave("/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/images/user_category_treemap.png", width = 8, height = 5, dpi = 300, bg = "white")


#Generating Pie chart of distribution of distance from each type of activity


# Step 1: Calculate the average for each activity type (excluding sedentaryActive)
avg_values <- df_act %>%
  summarise(
    veryActive = mean(veryActive_distance, na.rm = TRUE),
    moderatelyActive = mean(moderatelyActive_distance, na.rm = TRUE),
    lightActive = mean(lightActive_distance, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Activity", values_to = "Distance")

# Step 2: Prepare data for plotting
df_pie <- avg_values %>%
  arrange(desc(Distance)) %>%
  mutate(
    Distance = round(Distance, 1),  # Round to 1 decimal place
    label = paste0(Activity, "\n", Distance, " km")  # Format labels without percentages
  )

# Step 3: Create the pie chart
ggplot(df_pie, aes(x = "", y = Distance, fill = Activity)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5),  # Centers labels inside slices
            color = "black", size = 4) +
  scale_fill_manual(values = c("#FDCEC3", "#FDB2A1", "#F78D74")) +  # Only 3 colors since 1 category is removed
  coord_polar(theta = "y", start = pi / 2) +  # Rotate to proper alignment
  theme_void() +
  labs(title = "Average Activity Distance Breakdown", caption = "Avg distance distribution of activities") +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 11, hjust = 1, vjust = 1, color = "grey20"),
    legend.position = "none"
  )
ggsave("/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/images/user_activity_distance.png", width = 5, height = 5, dpi = 300, bg = "white")

#Generating the pie chart of sleep data

# Step 1: Create the data frame
df_pie <- data.frame(
  RowType = c("filled_rows", "na_rows"),
  Count = c(888, 764),
  records_percentage = c(53.8, 46.2)  # Already in percentage form
)

# Step 2: Create labels
df_sleep_filled$label <- paste0(df_pie$RowType, "\n", df_sleep_filled$records_percentage, "%")  # Format labels

# Step 3: Create the pie chart
ggplot(df_sleep_filled, aes(x = "", y = records_percentage, fill = RowType)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5),  # Centers text inside slices
            color = "black", size = 4) +
  scale_fill_manual(values = c("#CCE8CC", "#FEFADC")) +  # Assign colors
  coord_polar(theta = "y", start = pi / 2) +  # Ensure proper alignment
  theme_void() +
  labs(title = "Distribution of Filled Sleep records", caption = "Sleep Records Percentage") +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 11, hjust = 1, vjust = 1, color = "grey20"),
    legend.position = "none"  # Remove legend since labels are inside the pie
  )
ggsave("/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/images/user_filled_sleep.png", width = 5, height = 5, dpi = 300, bg = "white")

#Donut chart for Heavy Users

# Filter for "Heavy users"
df_donut_Heavy <- df_data %>%
  filter(user_label == "Heavy User") %>%
  filter(!is.na(sleep_range))  # Remove rows with NA in sleep_range

df_sumd <- df_donut_Heavy %>%
  count(sleep_range) %>%  # Count occurrences of each sleep_range
  arrange(desc(sleep_range)) %>%
  mutate(percent = round(100 * n / sum(n), 1),  # Use 'n' as count
         label = paste0(sleep_range, " (", percent, "%)"),
         cumulative = cumsum(n) - (n / 2))  # Use 'n' for midpoint calculation

ggplot(df_sumd, aes(x = 2, y = n, fill = sleep_range)) +  # Change 'count' to 'n'
  geom_bar(stat = "identity", width = 1) +
  geom_label(aes(y = cumulative, label = label),  
             fill = "white",  
             color = "black",  
             size = 4,
             label.size = 0.25) +  
  scale_fill_manual(values = c("#F78D74", "grey95")) +
  coord_polar(theta = "y") +
  xlim(0.4, 3) +  
  theme_void() +
  labs(x = "X-Axis Label", caption = "Daily sleep range") +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.caption = element_text(size = 11, hjust = 1, vjust= 12, color = "grey20"),
    legend.position = "none")  

str(df_donut_Heavy)

ggsave("/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/images/heavy_user_sleepRange.png", width = 8, height = 5, dpi = 300, bg = "white")

#Donut chart for Frequent Users

# Filter for "Frequent users"
df_donut_frequent <- df_data %>%
  filter(user_label == "Frequent User") %>%
  filter(!is.na(sleep_range))  # Remove rows with NA in sleep_range

df_sumf <- df_donut_frequent %>%
  group_by(sleep_range) %>%
  summarise(count = n()) %>%  # Count occurrences per sleep_range
  arrange(desc(sleep_range)) %>% 
  ungroup() %>%  # Ensure proper calculations
  mutate(
    count = as.numeric(count),  # ✅ Convert count to numeric
    percent = round(100 * count / sum(count), 1),  # Calculate percentages
    label = paste0(sleep_range, " (", percent, "%)"),  # Format: "Category (XX%)"
    cumulative = cumsum(count) - (count / 2)  # Midpoint for label placement
  )

ggplot(df_sumf, aes(x = 2, y = count, fill = sleep_range)) +
  geom_bar(stat = "identity", width = 1) +
  geom_label(aes(y = cumulative, label = label),  # Use cumulative to center labels
             fill = "white",  # Background for readability
             color = "black",  # Make labels bold
             size = 4,
             label.size = 0.25)+  # Adjust size for readability
  scale_fill_manual(values = c("#E78F8E", "grey95")) +
  coord_polar(theta = "y") +
  xlim(0.4, 3) +  # Adjust the limits to create the hole in the middle
  theme_void() +
  labs(x = "X-Axis Label", caption = "Daily sleep range") +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.caption = element_text(size = 11, hjust = 1, vjust= 12, color = "grey20"),
    legend.position = "none")  # Removes legend

ggsave("/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/images/frequent_user_sleepRange.png", width = 8, height = 5, dpi = 300, bg = "white")

#Donut chart for Moderate Users

# Filter for "Moderate users"
df_donut_Moderate <- df_data %>%
  filter(user_label == "Moderate User") %>%
  filter(!is.na(sleep_range))  # Remove rows with NA in sleep_range

df_summ <- df_donut_Moderate %>%
  group_by(sleep_range) %>%
  summarise(count = n()) %>%  # Count occurrences per sleep_range
  arrange(desc(sleep_range)) %>% 
  ungroup() %>%  # Ensure proper calculations
  mutate(
    count = as.numeric(count),  # ✅ Convert count to numeric
    percent = round(100 * count / sum(count), 1),  # Calculate percentages
    label = paste0(sleep_range, " (", percent, "%)"),  # Format: "Category (XX%)"
    cumulative = cumsum(count) - (count / 2)  # Midpoint for label placement
  )

ggplot(df_summ, aes(x = 2, y = count, fill = sleep_range)) +
  geom_bar(stat = "identity", width = 1) +
  geom_label(aes(y = cumulative, label = label),  # Use cumulative to center labels
             fill = "white",  # Background for readability
             color = "black",  # Make labels bold
             size = 4,
             label.size = 0.25)+  # Adjust size for readability
  scale_fill_manual(values = c("#CCE8CC", "grey95")) +
  coord_polar(theta = "y") +
  xlim(0.4, 3) +  # Adjust the limits to create the hole in the middle
  theme_void() +
  labs(x = "X-Axis Label", caption = "Daily sleep range") +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.caption = element_text(size = 11, hjust = 1, vjust= 12, color = "grey20"),
    legend.position = "none")  # Removes legend

ggsave("/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/images/moderate_user_sleepRange.png", width = 8, height = 5, dpi = 300, bg = "white")

# Filter for "Light users"
df_donut_light <- df_data %>%
  filter(user_label == "Light User") %>%
  filter(!is.na(sleep_range))  # Remove rows with NA in sleep_range

df_suml <- df_donut_light %>%
  group_by(sleep_range) %>%
  summarise(count = n()) %>%  # Count occurrences per sleep_range
  arrange(desc(sleep_range)) %>% 
  ungroup() %>%  # Ensure proper calculations
  mutate(
    count = as.numeric(count),  # ✅ Convert count to numeric
    percent = round(100 * count / sum(count), 1),  # Calculate percentages
    label = paste0(sleep_range, " (", percent, "%)"),  # Format: "Category (XX%)"
    cumulative = cumsum(count) - (count / 2)  # Midpoint for label placement
  )

ggplot(df_suml, aes(x = 2, y = count, fill = sleep_range)) +
  geom_bar(stat = "identity", width = 1) +
  geom_label(aes(y = cumulative, label = label),  # Use cumulative to center labels
             fill = "white",  # Background for readability
             color = "black",  # Make labels bold
             size = 4,
             label.size = 0.25)+  # Adjust size for readability
  scale_fill_manual(values = c("#FEFADC", "grey95")) +
  coord_polar(theta = "y") +
  xlim(0.4, 3) +  # Adjust the limits to create the hole in the middle
  theme_void() +
  labs(x = "X-Axis Label", caption = "Daily sleep range") +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.caption = element_text(size = 11, hjust = 1, vjust= 12, color = "grey20"),
    legend.position = "none")  # Removes legend

ggsave("/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/images/light_user_sleepRange.png", width = 8, height = 5, dpi = 300, bg = "white")
View(df_met)

#Visualization for Heavy User
df_filtered <- df_met %>%
  filter(user_label == "Heavy User")

# Aggregate METs by weekday and time (using mean)
heatmap_data <- df_filtered %>%
  group_by(weekday, time) %>%
  summarise(METs = mean(METs), .groups = "drop")

# Convert weekday to factor for proper ordering
heatmap_data$weekday <- factor(heatmap_data$weekday, 
                               levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Plot heatmap using ggplot2
ggplot(heatmap_data, aes(x = time, y = weekday, fill = METs)) +
  geom_tile(color = "white") +  # Add borders
  scale_fill_gradient(low = "grey95", high = "#F78D74") +  # Color gradient
  labs(title = "Heatmap of METs for Heavy Users", x = "Time", y = NULL, fill = "METs") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(face = "bold", size = 14))  # Rotate x-axis labels

ggsave("/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/images/heavy_user_MET.png", width = 5, height = 8, dpi = 300, bg = "white")

#Visualization for Frequent User
df_filtered_f <- df_met %>%
  filter(user_label == "Frequent User")


# Aggregate METs by weekday and time (using mean)
heatmap_data_f <- df_filtered_f %>%
  group_by(weekday, time) %>%
  summarise(METs = mean(METs), .groups = "drop")

# Convert weekday to factor for proper ordering
heatmap_data_f$weekday <- factor(heatmap_data_f$weekday, 
                                 levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Plot heatmap using ggplot2
ggplot(heatmap_data_f, aes(x = time, y = weekday, fill = METs)) +
  geom_tile(color = "white") +  # Add borders
  scale_fill_gradient(low = "grey95", high = "#E78F8E") +  # Color gradient
  labs(title = "Heatmap of METs for Frequent Users", x = "Time", y = NULL, fill = "METs") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(face = "bold", size = 14))  # Rotate x-axis labels

ggsave("/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/images/frequent_user_MET.png", width = 5, height = 8, dpi = 300, bg = "white")

#Visualization for Frequent User
df_filtered_m <- df_met %>%
  filter(user_label == "Moderate User")


# Aggregate METs by weekday and time (using mean)
heatmap_data_m <- df_filtered_m %>%
  group_by(weekday, time) %>%
  summarise(METs = mean(METs), .groups = "drop")

# Convert weekday to factor for proper ordering
heatmap_data_m$weekday <- factor(heatmap_data_m$weekday, 
                                 levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Plot heatmap using ggplot2
ggplot(heatmap_data_m, aes(x = time, y = weekday, fill = METs)) +
  geom_tile(color = "white") +  # Add borders
  scale_fill_gradient(low = "grey95", high = "#8FC08F") +  # Color gradient
  labs(title = "Heatmap of METs for Moderate Users", x = "Time", y = NULL, fill = "METs") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(face = "bold", size = 14))  # Rotate x-axis labels

ggsave("/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/images/moderate_user_MET.png", width = 5, height = 8, dpi = 300, bg = "white")

#Visualization for Light User
df_filtered_l <- df_met %>%
  filter(user_label == "Light User")


# Aggregate METs by weekday and time (using mean)
heatmap_data_l <- df_filtered_l %>%
  group_by(weekday, time) %>%
  summarise(METs = mean(METs), .groups = "drop")

# Convert weekday to factor for proper ordering
heatmap_data_l$weekday <- factor(heatmap_data_l$weekday, 
                                 levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Plot heatmap using ggplot2
ggplot(heatmap_data_l, aes(x = time, y = weekday, fill = METs)) +
  geom_tile(color = "white") +  # Add borders
  scale_fill_gradient(low = "grey95", high = "#F0DF64") +  # Color gradient
  labs(title = "Heatmap of METs for Light Users", x = "Time", y = NULL, fill = "METs") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(face = "bold", size = 14))  # Rotate x-axis labels

ggsave("/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/images/light_user_MET.png", width = 5, height = 8, dpi = 300, bg = "white")


# VISUALIZATION of average distance travelled By each user weekly

#Visualization for Heavy User
View(df_activity)
# Aggregate data: Calculate the average total_distance per weekday
df_avg_distance_h <- df_act %>%
  filter(user_label == "Heavy User") %>%  # Only include Heavy Users
  group_by(weekday) %>%
  summarise(avg_distance = mean(total_distance, na.rm = TRUE), .groups = "drop")

# Ensure weekdays are in correct order
df_avg_distance_h$weekday <- factor(df_avg_distance_h$weekday, 
                                    levels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                               "Friday", "Saturday", "Sunday"))

# Plot the histogram-like bar chart
ggplot(df_avg_distance_h, aes(x = weekday, y = avg_distance, fill = avg_distance)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.7) +  # Thicker bars
  labs(title = "Daily avg distance of heavy user", 
       x = "Weekday", 
       y = "Average Distance (km)") +
  scale_y_continuous(limits = c(0, max(df_avg_distance_h$avg_distance) * 1.3),  # More space above bars
                     expand = expansion(mult = c(0, 0.05))) +  # Prevent bars from touching the top
  scale_fill_gradient(low = "grey95", high = "#F78D74") +  # Gradient color similar to calories plot
  theme_minimal() +
  theme(plot.title = element_text(size = 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11),  # Rotate x-axis labels
        axis.text.y = element_text(size = 10),
        panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5))  # Add reference gridlines

ggsave("/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/images/heavy_user_distance_weekly.png", width = 8, height = 5, dpi = 300, bg = "white")

#Visualization for Frequent User

# Aggregate data: Calculate the average total_distance per weekday
df_avg_distance_f <- df_act %>%
  filter(user_label == "Frequent User") %>%  # Only include Heavy Users
  group_by(weekday) %>%
  summarise(avg_distance = mean(total_distance, na.rm = TRUE), .groups = "drop")

# Ensure weekdays are in correct order
df_avg_distance_f$weekday <- factor(df_avg_distance_f$weekday, 
                                    levels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                               "Friday", "Saturday", "Sunday"))

# Plot the histogram-like bar chart
ggplot(df_avg_distance_f, aes(x = weekday, y = avg_distance, fill = avg_distance)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.7) +  # Thicker bars
  labs(title = "Daily avg distance of frequent user", 
       x = "Weekday", 
       y = "Average Distance (km)") +
  scale_y_continuous(limits = c(0, max(df_avg_distance_f$avg_distance) * 1.3),  # More space above bars
                     expand = expansion(mult = c(0, 0.05))) +  # Prevent bars from touching the top
  scale_fill_gradient(low = "grey95", high = "#E78F8E") +  # Gradient color similar to calories plot
  theme_minimal() +
  theme(plot.title = element_text(size = 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11),  # Rotate x-axis labels
        axis.text.y = element_text(size = 10),
        panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5))  # Add reference gridlines

ggsave("/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/images/frequent_user_distance_weekly.png", width = 8, height = 5, dpi = 300, bg = "white")

#Visualization for Moderate User

# Aggregate data: Calculate the average total_distance per weekday
df_avg_distance_m <- df_act %>%
  filter(user_label == "Moderate User") %>%  # Only include Heavy Users
  group_by(weekday) %>%
  summarise(avg_distance = mean(total_distance, na.rm = TRUE), .groups = "drop")

# Ensure weekdays are in correct order
df_avg_distance_m$weekday <- factor(df_avg_distance_m$weekday, 
                                    levels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                               "Friday", "Saturday", "Sunday"))

# Plot the histogram-like bar chart
ggplot(df_avg_distance_m, aes(x = weekday, y = avg_distance, fill = avg_distance)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.7) +  # Thicker bars
  labs(title = "Daily avg distance of moderate user", 
       x = "Weekday", 
       y = "Average Distance (km)") +
  scale_y_continuous(limits = c(0, max(df_avg_distance_m$avg_distance) * 1.3),  # More space above bars
                     expand = expansion(mult = c(0, 0.05))) +  # Prevent bars from touching the top
  scale_fill_gradient(low = "grey95", high = "#8FC08F") +  # Gradient color similar to calories plot
  theme_minimal() +
  theme(plot.title = element_text(size = 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11),  # Rotate x-axis labels
        axis.text.y = element_text(size = 10),
        panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5))  # Add reference gridlines

ggsave("/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/images/moderate_user_distance_weekly.png", width = 8, height = 5, dpi = 300, bg = "white")

#Visualization for Light User

# Aggregate data: Calculate the average total_distance per weekday
df_avg_distance_l <- df_act %>%
  filter(user_label == "Light User") %>%  # Only include Heavy Users
  group_by(weekday) %>%
  summarise(avg_distance = mean(total_distance, na.rm = TRUE), .groups = "drop")

# Ensure weekdays are in correct order
df_avg_distance_l$weekday <- factor(df_avg_distance_l$weekday, 
                                    levels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                               "Friday", "Saturday", "Sunday"))

# Plot the histogram-like bar chart
ggplot(df_avg_distance_m, aes(x = weekday, y = avg_distance, fill = avg_distance)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.7) +  # Thicker bars
  labs(title = "Daily avg distance of light user", 
       x = "Weekday", 
       y = "Average Distance (km)") +
  scale_y_continuous(limits = c(0, max(df_avg_distance_m$avg_distance) * 1.3),  # More space above bars
                     expand = expansion(mult = c(0, 0.05))) +  # Prevent bars from touching the top
  scale_fill_gradient(low = "grey95", high = "#F0DF64") +  # Gradient color similar to calories plot
  theme_minimal() +
  theme(plot.title = element_text(size = 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11),  # Rotate x-axis labels
        axis.text.y = element_text(size = 10),
        panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5))  # Add reference gridlines

ggsave("/Users/farhanhauzannadhir/Documents/R_Projects/Capstone Project Coursera DA/images/light_user_distance_weekly.png", width = 8, height = 5, dpi = 300, bg = "white")

