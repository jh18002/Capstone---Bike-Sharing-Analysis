install.packages("tidyverse")
library(tidyverse)
install.packages("readr")
library(readr)
install.packages("here")
library(here)
install.packages("skimr")
library(skimr)
install.packages("janitor")
library(janitor)
library(lubridate)
library(hms)
install.packages("geosphere")
library(geosphere)
install.packages("scales")
library(scales)

# Define Mode function
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

# Import all 12 months csv files
bike_trip_202106 <- read_csv("202106-divvy-tripdata.csv")
bike_trip_202107 <- read_csv("202107-divvy-tripdata.csv")
bike_trip_202108 <- read_csv("202108-divvy-tripdata.csv")
bike_trip_202109 <- read_csv("202109-divvy-tripdata.csv")
bike_trip_202110 <- read_csv("202110-divvy-tripdata.csv")
bike_trip_202111 <- read_csv("202111-divvy-tripdata.csv")
bike_trip_202112 <- read_csv("202112-divvy-tripdata.csv")
bike_trip_202201 <- read_csv("202201-divvy-tripdata.csv")
bike_trip_202202 <- read_csv("202202-divvy-tripdata.csv")
bike_trip_202203 <- read_csv("202203-divvy-tripdata.csv")
bike_trip_202204 <- read_csv("202204-divvy-tripdata.csv")
bike_trip_202205 <- read_csv("202205-divvy-tripdata.csv")

# Inspect the imported
colnames(bike_trip_202106)
colnames(bike_trip_202107)
colnames(bike_trip_202108)
colnames(bike_trip_202109)
colnames(bike_trip_202110)
colnames(bike_trip_202111)
colnames(bike_trip_202112)
colnames(bike_trip_202201)
colnames(bike_trip_202202)
colnames(bike_trip_202203)
colnames(bike_trip_202204)
colnames(bike_trip_202205)

# Merge all 12 months worth of df
bike_trip_12mo <- rbind(bike_trip_202106, bike_trip_202107, bike_trip_202108,
                        bike_trip_202109, bike_trip_202110, bike_trip_202111,
                        bike_trip_202112, bike_trip_202201, bike_trip_202202,
                        bike_trip_202203, bike_trip_202204, bike_trip_202205)

# Remove all temp CSV df
remove(bike_trip_202106, bike_trip_202107, bike_trip_202108, bike_trip_202109, 
   bike_trip_202110, bike_trip_202111, bike_trip_202112, bike_trip_202201, 
   bike_trip_202202, bike_trip_202203, bike_trip_202204, bike_trip_202205)

# Check dataframe structure
colnames(bike_trip_12mo) # List of cols
nrow(bike_trip_12mo) # Num of rows
dim(bike_trip_12mo) # dimensions of the df
head(bike_trip_12mo) # Check 1st 6 rows
str(bike_trip_12mo) # See list of columns and data types (numeric, character, etc)
summary(bike_trip_12mo) # Statistical summary of data. Mainly for numeric

# Check value variation in a column
unique(bike_trip_12mo$member_casual)
unique(bike_trip_12mo$rideable_type)

# Add ride_length column
bike_trip_12mo$ride_length <- difftime(bike_trip_12mo$ended_at, bike_trip_12mo$started_at)

# Clean ride_length for the negative ride length
bike_trip_12mo[bike_trip_12mo$ride_length < 0,]

bike_trip_12mo_cleaned <- bike_trip_12mo %>% 
  filter(ride_length >= 0)

bike_trip_12mo_cleaned[bike_trip_12mo_cleaned$ride_length < 0,]

# Format ride_length from difftime to numeric
is.factor(bike_trip_12mo_cleaned$ride_length)
bike_trip_12mo_cleaned$ride_length <- as.numeric(as.character(bike_trip_12mo_cleaned$ride_length))
is.numeric(bike_trip_12mo_cleaned$ride_length)
# Format ride_length to HH:MM:SS
#bike_trip_12mo_cleaned$ride_length <- hms(seconds_to_period(bike_trip_12mo_cleaned$ride_length))

#summary(bike_trip_12mo_cleaned)
#ride_length     
#Min.   :      0  
#1st Qu.:    382  
#Median :    680  
#Mean   :   1241  
#3rd Qu.:   1236  
#Max.   :3356649  

# Remove unclean df
remove(bike_trip_12mo)
                       
# Add day_of_week column
bike_trip_12mo_cleaned$day_of_week <- wday(bike_trip_12mo_cleaned$started_at)

# Day of week in full text (not abbr)
#bike_trip_12mo_cleaned$day_of_week <- wday(bike_trip_12mo_cleaned$started_at, label=TRUE, abbr=FALSE)

# Add distance variable which derived from start_lng, start_lat, end_lng, and end_lat (in that order)
bike_trip_12mo_cleaned <- bike_trip_12mo_cleaned %>% 
  rowwise %>% 
  mutate(trip_distance = as.vector(distm(x = c(start_lng, start_lat), y = c(end_lng, end_lat), fun = distHaversine)))

# (NOT NEED: For statistic accuracy purpose) Clean/replace NA value in trip_distance
#bike_trip_12mo_cleaned[is.na(bike_trip_12mo_cleaned$trip_distance),]
#bike_trip_12mo_cleaned$trip_distance <- replace_na(bike_trip_12mo_cleaned$trip_distance, 0)

# Descriptive Analysis
summary(bike_trip_12mo_cleaned$ride_length)
table(bike_trip_12mo_cleaned$member_casual)
ride_length_mean <- mean(bike_trip_12mo_cleaned$ride_length)
ride_length_median <- median(bike_trip_12mo_cleaned$ride_length)
day_of_week_mode <- find_mode(bike_trip_12mo_cleaned$day_of_week)
ride_length_max <- max(bike_trip_12mo_cleaned$ride_length)
ride_length_min <- min(bike_trip_12mo_cleaned$ride_length)

# Pivot varies information
# Avg ride_length for each type of users
avg_ride_length_usertype <- aggregate(bike_trip_12mo_cleaned$ride_length, 
          by = list(bike_trip_12mo_cleaned$member_casual), 
          FUN = mean)

colnames(avg_ride_length_usertype) <- c("member_casual", "avg_ride_length")

# Avg ride_length for each type of users by day_of_week (will export this)
avg_ride_length_usertype_dayofweek <- aggregate(bike_trip_12mo_cleaned$ride_length, 
          by = list(bike_trip_12mo_cleaned$member_casual, bike_trip_12mo_cleaned$day_of_week), 
          FUN = mean)

colnames(avg_ride_length_usertype_dayofweek) <- c("member_casual", "day_of_week", "avg_ride_length")

avg_ride_length_usertype_dayofweek <- avg_ride_length_usertype_dayofweek %>% 
  mutate(day_of_week = wday(day_of_week, label=TRUE, abbr=FALSE))

# Avg ride_length for each type of users by bike type and day_of_week (will export this)
avg_ride_length_usertype_biketype_dayofweek <- aggregate(bike_trip_12mo_cleaned$ride_length, 
                                                by = list(bike_trip_12mo_cleaned$member_casual,
                                                          bike_trip_12mo_cleaned$rideable_type,
                                                          bike_trip_12mo_cleaned$day_of_week), 
                                                FUN = mean)

colnames(avg_ride_length_usertype_biketype_dayofweek) <- c("member_casual", "rideable_type", "day_of_week", "avg_ride_length")

avg_ride_length_usertype_biketype_dayofweek <- avg_ride_length_usertype_biketype_dayofweek %>% 
  mutate(day_of_week = wday(day_of_week, label=TRUE, abbr=FALSE))

# Count of riders for each type of users by day_of_week
count_rides_usertype_dayofweek <- bike_trip_12mo_cleaned %>% 
  count(member_casual, day_of_week, name = "number_of_rides")

count_rides_usertype_dayofweek <- count_rides_usertype_dayofweek %>% 
  mutate(day_of_week = wday(day_of_week, label=TRUE, abbr=FALSE))

# Count of riders for each type of users by bike type and day_of_week
count_rides_usertype_biketype_dayofweek <- bike_trip_12mo_cleaned %>% 
  count(member_casual, rideable_type, day_of_week, name = "number_of_rides")

count_rides_usertype_biketype_dayofweek <- count_rides_usertype_biketype_dayofweek %>% 
  mutate(day_of_week = wday(day_of_week, label=TRUE, abbr=FALSE))

# Alternative way to count
#bike_trip_12mo_cleaned %>% 
#  group_by(member_casual, day_of_week) %>% 
#  summarize(number_of_rides = n()) %>% 
#  arrange(member_casual, day_of_week)

# Avg, min, and max trip distance for each type of users by day_of_week
bike_trip_distance_stat <- bike_trip_12mo_cleaned %>% 
  mutate(day_of_week = wday(started_at, label = TRUE, abbr = FALSE)) %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(average_trip_distance = mean(trip_distance, na.rm = TRUE),
            max_trip_distance = max(trip_distance, na.rm = TRUE),
            min_trip_distance = min(trip_distance, na.rm = TRUE)) %>% 
  arrange(member_casual, day_of_week)

# Control check
j <- bike_trip_12mo_cleaned[bike_trip_12mo_cleaned$trip_distance >= 1190854 & is.na(bike_trip_12mo_cleaned$trip_distance) == FALSE, ]
i <- bike_trip_12mo_cleaned[bike_trip_12mo_cleaned$trip_distance >= 114511 & is.na(bike_trip_12mo_cleaned$trip_distance) == FALSE, ]
bike_trip_12mo_cleaned[bike_trip_12mo_cleaned$member_casual == "casual" & bike_trip_12mo_cleaned$rideable_type == "docked_bike", ]

View(i)
View(j)
rm(i)
rm(j)


# Alternative way to min/max with NA value in the data frame (to ignore it)
#max(bike_trip_12mo_cleaned$trip_distance, na.rm = TRUE)
#min(bike_trip_12mo_cleaned$trip_distance, na.rm = TRUE)

# Min/Max for Matrix
#which(bike_trip_12mo_cleaned$trip_distance == max(bike_trip_12mo_cleaned$trip_distance, na.rm = TRUE), arr.ind = TRUE)
#which(bike_trip_12mo_cleaned$trip_distance == min(bike_trip_12mo_cleaned$trip_distance, na.rm = TRUE), arr.ind = TRUE)

# Visualization for number of rides by rider type and day-of-week
count_rides_usertype_dayofweek %>% 
  ggplot(aes(x = day_of_week, 
             y = number_of_rides, 
             group = member_casual, 
             label = number_of_rides)) +
  geom_line(aes(color = member_casual)) + 
  geom_point(aes(color = member_casual)) +
  geom_text(nudge_y = 15000,
            size = 3) +
  scale_y_continuous(labels = comma) +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(title = "Number of Rides", 
       subtitle = "By Rider Type and Day-of-Week",
       x = "Day of the Week",
       y = "Number of Rides")

count_rides_usertype_biketype_dayofweek %>% 
  ggplot(aes(x = rideable_type, 
             y = number_of_rides, 
             group = member_casual, 
             label = number_of_rides)) +
  geom_line(aes(color = member_casual)) + 
  geom_point(aes(color = member_casual)) +
  geom_text(nudge_y = 15000,
            size = 3, 
            check_overlap = TRUE) +
  facet_wrap(vars(day_of_week), ncol = 3) +
  scale_y_continuous(labels = comma) +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(title = "Number of Rides", 
       subtitle = "By Rider Type, Bike Type, and Day-of-Week",
       x = "Bike Type",
       y = "Number of Rides")

count_rides_usertype_biketype_dayofweek %>% 
  ggplot(aes(x = member_casual, 
             y = number_of_rides,
             fill = rideable_type)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = comma,
                     expand = c(0, 0)) +
  facet_wrap(vars(day_of_week), ncol = 3) +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(title = "Number of Rides", 
       subtitle = "By Rider Type, Bike Type, and Day-of-Week",
       x = "Rider Type",
       y = "Number of Rides")

# Control Check - Visualization for number of rides by rider type
bike_trip_12mo_cleaned %>% 
  mutate(day_of_week = wday(started_at, label = TRUE, abbr = FALSE)) %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, 
             y = number_of_rides, 
             group = member_casual,
             label = member_casual)) +
  geom_line(aes(color = member_casual)) +
  geom_point(aes(color = member_casual)) +
  geom_text(nudge_y = 15000) +
  scale_y_continuous(labels = comma) +
  theme_classic() +
  theme(legend.position = "right") +
  labs(title = "Number of Rides", 
       subtitle = "By Rider Type and Day-of-Week",
       x = "Day of the Week",
       y = "Number of Rides")

# Visualization for average duration
avg_ride_length_usertype_dayofweek %>% 
  ggplot(aes(x = day_of_week, 
             y = avg_ride_length,
             fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = comma,
                     expand = c(0, 0)) +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(title = "Average Duration", 
       subtitle = "By Rider Type and Day-of-Week",
       x = "Day of the Week",
       y = "Avg Ride Length (in seconds)")

avg_ride_length_usertype_biketype_dayofweek %>% 
  ggplot(aes(x = day_of_week, 
             y = avg_ride_length,
             fill = rideable_type)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = comma,
                     expand = c(0, 0)) +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(title = "Average Duration", 
       subtitle = "By Rider Type, Bike Type, and Day-of-Week",
       x = "Day of the Week",
       y = "Avg Ride Length (in seconds)")

# Control Check - Visualization for average duration
bike_trip_12mo_cleaned %>% 
  mutate(day_of_week = wday(started_at, label = TRUE, abbr = FALSE)) %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(),
            avg_ride_length = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = avg_ride_length, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = comma) +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(title = "Average Duration", 
       subtitle = "By Rider Type and Day-of-Week",
       x = "Day of the Week",
       y = "Avg Ride Length (in seconds)")

# Visualization for AVG Trip Distance
bike_trip_distance_stat %>% 
  ggplot(aes(x = day_of_week, 
             y = average_trip_distance, 
             fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = comma,
                     expand = c(0, 0)) +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(title = "Average Trip Distance", 
       subtitle = "By Rider Type and Day-of-Week",
       x = "Day of the Week",
       y = "Avg Ride Length (in seconds)")

# Visualization for MAX Trip Distance
bike_trip_distance_stat %>% 
  ggplot(aes(x = day_of_week, y = max_trip_distance, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge")

# Visualization for MIN Trip Distance
bike_trip_distance_stat %>% 
  ggplot(aes(x = day_of_week, y = min_trip_distance, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge")

# Export varies summary information to CSV files
write_csv(bike_trip_12mo_cleaned, "bike_trip_12mo_cleaned.csv")

#avg_ride_length_usertype$avg_ride_length <- hms(seconds_to_period(avg_ride_length_usertype$avg_ride_length))
write_csv(avg_ride_length_usertype, "avg_ride_length_usertype.csv")

#avg_ride_length_usertype$ride_length <- hms(seconds_to_period(avg_ride_length_usertype$avg_ride_length))
write_csv(avg_ride_length_usertype_dayofweek, "avg_ride_length_usertype_dayofweek.csv")

write_csv(count_rides_usertype_dayofweek, "count_rides_usertype_dayofweek.csv")
write_csv(bike_trip_distance_stat, "bike_trip_distance_stat.csv")

