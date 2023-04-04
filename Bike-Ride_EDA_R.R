library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)

all_trips  <- read.csv("all_trips.csv")

all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$trip_duration<0),] %>% 
  select(-"X")

aggregate(all_trips_v2$trip_duration ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$trip_duration ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$trip_duration ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$trip_duration ~ all_trips_v2$member_casual, FUN = min)


aggregate(all_trips_v2$trip_duration ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean) 
  
all_trips_v2 %>% 
  mutate(Weekday=wday(start_time,label=TRUE)) %>% 
  group_by(member_casual,Weekday) %>% 
  summarise(number_of_rides=n(),average_duration=mean(trip_duration)) %>% 
  arrange(member_casual,Weekday) %>% 
  ggplot(aes(x=Weekday,y=number_of_rides,fill=member_casual))+geom_col(position="dodge")

all_trips_v2 %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(),average_duration = mean(trip_duration)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
 