install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")

library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #for data visualization
getwd() #displays working directory

#I uploaded the data and renamed it.
q2_2019 <- read_csv("Divvy_Trips_2019_Q2.csv")
q3_2019 <- read_csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <- read_csv("Divvy_Trips_2019_Q4.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")

#Checked column names to make sure they all matched
colnames(q3_2019)
colnames(q4_2019)
colnames(q2_2019)
colnames(q1_2020)

#Found inconsistencies. Code below makes colnames consistent

(q4_2019 <- rename(q4_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))

(q3_2019 <- rename(q3_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))

(q2_2019 <- rename(q2_2019
                   ,ride_id = "01 - Rental Details Rental ID"
                   ,rideable_type = "01 - Rental Details Bike ID" 
                   ,started_at = "01 - Rental Details Local Start Time"  
                   ,ended_at = "01 - Rental Details Local End Time"  
                   ,start_station_name = "03 - Rental Start Station Name" 
                   ,start_station_id = "03 - Rental Start Station ID"
                   ,end_station_name = "02 - Rental End Station Name" 
                   ,end_station_id = "02 - Rental End Station ID"
                   ,member_casual = "User Type"))

# Inspected dataframes to look for inconsistencies

str(q1_2020)
str(q4_2019)
str(q3_2019)
str(q2_2019)

#Changed ride_id and rideable_id to character so they can stack correctly

q4_2019 <-  mutate(q4_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q3_2019 <-  mutate(q3_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q2_2019 <-  mutate(q2_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 

#Combined individual data sets to one big frame

all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)

#Removed column such as lat, long, birthyear and gender since they are irrelevant 

all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender,
            "01 - Rental Details Duration In Seconds Uncapped",
            "05 - Member Details Member Birthday Year",
            "Member Gender", "tripduration"))

#Noticed that once I combined the files, Members were listed as both "Member" and "Subscriber"
#Casual riders were listed as both "Customer and Casual"
#Changed the code so that they are consistent at "Member" and "Casusal"

all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual")) 

#Upon further inspection, I noticed that I can only aggregate the file at ride level
#I added date column so we can aggregate the data further

all_trips$date <- as.Date(all_trips$started_at) #default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

#Added a ride_length column which calculates difference between time trip started and ended

all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

#Converted ride_length to numeric so further calculations could be done

all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))

#Removed data entries where start_station_name is HQ (These are done for QC)
#Removed data entries where ride lenght is less than zero
#Moved data to a new table

all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

#Found mean, median, max, min

mean(all_trips_v2$ride_length) 
median(all_trips_v2$ride_length) 
max(all_trips_v2$ride_length) 
min(all_trips_v2$ride_length) 

#Summarize information above

summary(all_trips_v2$ride_length)

#Compare members and casual users based on information above

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

#Arranged days of week inorder so we can see the average ride time by each day for members vs casual users

all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
 
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

#Analyze ridership data by type and weekday

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()		#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		
  arrange(member_casual, weekday)			# sorts first by member/casual then by day of week.

#Created visualization of the number of rides by rider type

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

#Created a visualization for average duration

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")


