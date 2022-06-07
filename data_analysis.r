library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)

q2_2019 <- read.csv("/Users/Nick/Desktop/Divvy_2019/Data/Divvy_Trips_2019_Q2.csv")
q3_2019 <- read.csv("/Users/Nick/Desktop/Divvy_2019/Data/Divvy_Trips_2019_Q3.csv")
q4_2019 <- read.csv("/Users/Nick/Desktop/Divvy_2019/Data/Divvy_Trips_2019_Q4.csv")
q1_2020 <- read.csv("/Users/Nick/Desktop/Divvy_2019/Data/Divvy_Trips_2020_Q1.csv")

#renaming columns so they are all uniform.
q4_2019 <- rename(q4_2019,
                  ride_id = trip_id,
                  rideable_type = bikeid,
                  started_at = start_time,
                  ended_at = end_time,
                  start_station_name = from_station_name,
                  start_station_id = from_station_id,
                  end_station_name = to_station_name,
                  end_station_id = to_station_id,
                  member_casual = usertype)

q3_2019 <- rename(q3_2019,
                  ride_id = trip_id,
                  rideable_type = bikeid,
                  started_at = start_time,
                  ended_at = end_time,
                  start_station_name = from_station_name,
                  start_station_id = from_station_id,
                  end_station_name = to_station_name,
                  end_station_id = to_station_id,
                  member_casual = usertype)

q2_2019 <- rename(q2_2019,
                  ride_id = X01...Rental.Details.Rental.ID,
                  rideable_type = X01...Rental.Details.Bike.ID,
                  started_at = X01...Rental.Details.Local.Start.Time,
                  ended_at = X01...Rental.Details.Local.End.Time,
                  start_station_name = X03...Rental.Start.Station.Name,
                  start_station_id = X03...Rental.Start.Station.ID,
                  end_station_name = X02...Rental.End.Station.Name,
                  end_station_id = X02...Rental.End.Station.ID,
                  member_casual = User.Type)

#converting value types to match Q4_2020 so when combined, the columns stack correctly.
q4_2019 <- mutate(q4_2019, ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))

q3_2019 <- mutate(q3_2019, ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))

q2_2019 <- mutate(q2_2019, ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))

#Merges the 4 data frames into 1.
all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)

#Removing columns that didn't carry over from 2019 to 2020 and other data not in main objective.
all_trips <- all_trips %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender,
            X05...Member.Details.Member.Birthday.Year, Member.Gender,
            X01...Rental.Details.Duration.In.Seconds.Uncapped, tripduration))

#For the member_casual column, there are four classifications, when there should only be two. This renames Subscriber and Customer to member and casual to fix it.
all_trips <- all_trips %>% 
  mutate(member_casual = recode(member_casual,
                                "Subscriber" = "member",
                                "Customer" = "casual"))

#adding columns that list date/month/year/day of week
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

#adding ride length column.
all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at)

#convert ride_length from factor to numeric.
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))

#creating new data frame since we are dropping rows: Bikes taken out of service during the year and the HQ QR stations.
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length < 0),]

#Renaming numeric months to string names for easier visual labeling in Tableau
all_trips_v2 <- all_trips_v2 %>% 
  mutate(month = recode(month,
                        "01" = "January",
                        "02" = "February",
                        "03" = "March",
                        "04" = "April",
                        "05" = "May",
                        "06" = "June",
                        "07" = "July",
                        "08" = "August",
                        "09" = "September",
                        "10" = "October",
                        "11" = "November",
                        "12" = "December"))

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
##   all_trips_v2$member_casual all_trips_v2$ride_length
## 1                     casual                3552.7941
## 2                     member                 850.0783
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
##   all_trips_v2$member_casual all_trips_v2$ride_length
## 1                     casual                     1546
## 2                     member                      589
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
##   all_trips_v2$member_casual all_trips_v2$ride_length
## 1                     casual                  9383424
## 2                     member                  9056634
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)
##   all_trips_v2$member_casual all_trips_v2$ride_length
## 1                     casual                        2
## 2                     member                        1
#Creating the count for total trips at each station, for ending, and starting stations.
count_station_id <- table(all_trips_v2$start_station_id)
count_stop_id<- table(all_trips_v2$end_station_id)

#exporting .csv to re-upload as data frames 
write.csv(count_station_id, "C:/Users/Nick/Desktop/Divvy_2019/Saved/count_station_id.csv")
write.csv(count_stop_id, "C:/Users/Nick/Desktop/Divvy_2019/Saved/count_stop_id.csv")

#exported all_stations and manipulated data further in excel, re-uploaded as station_id_totals
all_stations <- bind_rows(count_station_id, count_stop_id)

station_id_totals <- read.csv("/Users/Nick/Desktop/Divvy_2019/Saved/station_id_totals.csv")

#isolating top 10 busiest stations for graphing
df <- station_id_totals %>% 
  arrange(desc(total_combined)) %>% 
  slice(1:10) 

#attempted to make a few charts with that data, but it was coming out too difficult to understand/read because of the continuous scaling for the station id's. Transformed station id's to characters to fix that.
df_v2 <- transform(df, station_id = as.character(station_id)) %>% 
  arrange(desc(total_combined))

  #A look at Casual vs Members vs Time spent on bikes.
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = 'dodge') +
  scale_y_continuous(labels = comma) +
  labs(title = "Average number of rides/day over last 12 months")
## `summarise()` has grouped output by 'member_casual'. You can override using the `.groups` argument.

#Top 10 busiest stations
df_v2 %>% 
  mutate(station_id = fct_reorder(station_id, total_combined)) %>% 
  ggplot(aes(x = station_id, y = total_combined, fill = station_id)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(type = "seq", palette = "Paired", direction = 1) +
  labs( y = "Total rides per station",
        x = "Station Id",
        title = "Top 10 Most Popular Stations",
        subtitle = "1.6% of stations, accounting for over 11% of all traffic")
