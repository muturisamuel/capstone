#### ______________________capstone Project _______________________________
##the objective of this script is to combine the data from 2020 and 2021 so as 
 ## to answer the how the casual members and members compare differently.
##installing packages

install.packages("tidyverse")
install.packages("lubridate")
install.packages("hms")
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(hms)

##____setting the work directory_________
setwd("D:/DATA ANALYSIS/GOOGLE DATA ANALYTICS/cyclist_data")

##____________uploading the data set____________________
JAN_2021 <- read.csv("202101-divvy-tripdata.csv")
FEB_2021 <- read.csv("202102-divvy-tripdata.csv")
MARCH_2021 <- read.csv("202103-divvy-tripdata.csv")
APR_2021 <- read.csv("202104-divvy-tripdata.csv")
MAY_2021 <-read.csv("202105-divvy-tripdata.csv")
JUNE_2021 <- read.csv("202106-divvy-tripdata.csv")
JULY_2021 <- read.csv("202107-divvy-tripdata.csv")
AUG_2021 <- read.csv("202108-divvy-tripdata.csv")
SEP_2021 <- read.csv("202109-divvy-tripdata.csv")
OCT_2021 <- read.csv("202110-divvy-tripdata.csv")
NOV_2021 <- read.csv("202111-divvy-tripdata.csv")
DEC_2021 <- read.csv("202112-divvy-tripdata.csv")
q1_2020 <-read.csv("Divvy_Trips_2020_Q1.csv")
apr_2020<- read.csv("202004-divvy-tripdata.csv")
may_2020 <- read.csv("202005-divvy-tripdata.csv")
june_2020 <- read.csv("202006-divvy-tripdata.csv")
july_2020 <-read.csv("202007-divvy-tripdata.csv")
aug_2020  <- read.csv("202008-divvy-tripdata.csv")
sep_2020 <- read.csv("202009-divvy-tripdata.csv")
oct_2020 <- read.csv("202010-divvy-tripdata.csv")
nov_2020 <- read.csv("202011-divvy-tripdata.csv")
dec_2020 <- read.csv("202012-divvy-tripdata.csv")
trip_data_2021 <- read.csv("trips2021v2.csv")

##inspecting the col names for any inconsistency
colnames(nov_2020)
colnames(NOV_2021)
colnames(dec_2020)
colnames(DEC_2021)
colnames(FEB_2021)
colnames(q1_2020)

##since there is no inconsistency in the col names let inspect the data frames in the data frames
str(q1_2020)
str(FEB_2021)
str(JAN_2021)
str(dec_2020)
str(MARCH_2021)
str(apr_2020)
str(may_2020)
str(MAY_2021)

##to enable data to stack propely we need to convert the start_station_id and end_station_id to character in the 2020 data sets 
q1_2020 <- mutate(q1_2020,start_station_id=as.character(start_station_id),end_station_id=as.character(end_station_id))
apr_2020 <- mutate(apr_2020,start_station_id=as.character(start_station_id),end_station_id=as.character(end_station_id))
may_2020 <- mutate(may_2020,start_station_id=as.character(start_station_id),end_station_id=as.character(end_station_id))
june_2020 <- mutate(june_2020,start_station_id=as.character(start_station_id),end_station_id=as.character(end_station_id))
july_2020 <- mutate(july_2020,start_station_id=as.character(start_station_id),end_station_id=as.character(end_station_id))
aug_2020 <-mutate(aug_2020,start_station_id=as.character(start_station_id),end_station_id=as.character(end_station_id))
sep_2020 <- mutate(sep_2020,start_station_id=as.character(start_station_id),end_station_id=as.character(end_station_id))
oct_2020 <- mutate(oct_2020,start_station_id=as.character(start_station_id),end_station_id=as.character(end_station_id))
nov_2020 <- mutate(nov_2020,start_station_id=as.character(start_station_id),end_station_id=as.character(end_station_id))
dec_2020 <- mutate(dec_2020,start_station_id=as.character(start_station_id),end_station_id=as.character(end_station_id))

## combining the 2020 data and 2021 data set 

trips_data <-bind_rows(JAN_2021,FEB_2021,MARCH_2021,APR_2021,MAY_2021,JUNE_2021,JULY_2021,AUG_2021,SEP_2021,OCT_2021,NOV_2021,DEC_2021,q1_2020,apr_2020,may_2020,june_2020,july_2020,aug_2020,sep_2020,oct_2020,nov_2020,dec_2020)


##____________dropping variables_________________

trips_data <- trips_data %>% 
  select(-c (start_lat, start_lng, end_lat, end_lng,ride_id,start_station_id,end_station_id))
str(trips_data)

##_______________cleaning data and preparing data for analysis____________ 
##________inspecting the new data frame_____

colnames(trips_data)
nrow(trips_data)
head(trips_data)
summarise(trips_data)
summary(trips_data)

##_________________making the col name member_casual to have two variables instead of four variables
table(trips_data$member_casual)
table(trips_data$started_at)

##____________we need to add more column that calculate the time,hour day,week,month and year for all data frames 
trips_data$date <- as.Date(trips_data$started_at)
trips_data$month <- format(as.Date(trips_data$date),"%b")
trips_data$DAY <- format(as.Date(trips_data$date),"%d")
trips_data$year <-format(as.Date(trips_data$date),"%Y")
trips_data$time <- format(as.POSIXct(trips_data$started_at),format="%H:%M:%S")
trips_data$day_of_week <- format(as.Date(trips_data$date),"%A")
trip_data$time <- as_hms(trip_data$time)
trip_data$hour <- hour(trip_data$time)

trips_data <- trips_data %>% 
select(-c (time))

##adding a ride_time calculated in seconds 
trips_data$ride_time <- difftime(trips_data$ended_at,trips_data$started_at)

##__converting ride length to minutes from secs___
str(trip_data$ride_time)
trip_data$ride_length <- difftime(trip_data$ended_at,trip_data$started_at,units = "mins")

##we need to inspect if all the columns have been added in the data 
str(trips_data)

##we have discovered that the ride_time is to numeric 
is.factor(trips_data$ride_time)
trips_data$ride_time <- as.numeric(as.character(trips_data$ride_time))
is.numeric(trips_data$ride_time)


##___creating column for different time of the day___
trip_data <- trip_data %>% mutate(time_of_day=
                                    case_when(hour == "0" ~ "Night",
                                              hour == "1" ~ "Night",
                                              hour == "2" ~ "Night",
                                              hour == "3" ~ "Night",
                                              hour == "4" ~ "Night",
                                              hour == "5" ~ "Night",
                                              hour == "6" ~ "Morning",
                                              hour == "7" ~ "Morning",
                                              hour == "8" ~ "Morning",
                                              hour == "9" ~ "Morning",
                                              hour == "10" ~ "Morning",
                                              hour == "11" ~ "Morning",
                                              hour == "12" ~ "Afternoon",
                                              hour == "13" ~ "Afternoon",
                                              hour == "14" ~ "Afternoon",
                                              hour == "15" ~ "Afternoon",
                                              hour == "16" ~ "Afternoon",
                                              hour == "17" ~ "Afternoon",
                                              hour == "18" ~ "Evening",
                                              hour == "19" ~ "Evening",
                                              hour == "20" ~ "Evening",
                                              hour == "21" ~ "Evening",
                                              hour == "22" ~ "Evening",
                                              hour == "23" ~ "Evening"))
##adding a month column in number format 

trip_data$month1 <- format(as.Date(trip_data$date),"%m")
trip_data <-trip_data %>%
  select(-c(month1))
str(trip_data)
##______adding a column for season types____
trip_data <- trip_data %>% mutate(season=
                                    case_when(month1 == "03" ~ "Spring",
                                              month1 == "04" ~ "Spring",
                                              month1 == "05" ~ "Spring",
                                              month1 == "06"  ~ "Summer",
                                              month1 == "07"  ~ "Summer",
                                              month1 == "08"  ~ "Summer",
                                              month1 == "09" ~ "Fall",
                                              month1 == "10" ~ "Fall",
                                              month1 == "11" ~ "Fall",
                                              month1 == "12" ~ "Winter",
                                              month1 == "01" ~ "Winter",
                                              month1 == "02" ~ "Winter"))                                              
##_______cleaning the data frame_________
###creating a new data frame since the some entries indicate that the some bikes were taken out of docks and ride length is negative
trip_data <- na.omit(trip_data)
trip_data <- distinct(trip_data)
trips_data_v2 <- trips_data[!(trips_data$start_station_name=="HQ QR"|trips_data$ride_time<0),]
str(trips_data_v2)

##step4:doing a descriptive analysis to have an overview of the data

##descriptive statistics of the ride_time 
summary(trips_data_v2$ride_time)
max(trips_data_v2$ride_time)

##comparing the casual and members users
aggregate(trips_data_v2$ride_time~trips_data_v2$member_casual,FUN =mean)
aggregate(trips_data_v2$ride_time~trips_data_v2$member_casual,FUN = median)
aggregate(trips_data_v2$ride_time~trips_data_v2$member_casual,FUN = max)
aggregate(trips_data_v2$ride_time~trips_data_v2$member_casual,FUN =min)

##average ride time by each day for members vs casual users 
aggregate(trips_data_v2$ride_time~trips_data_v2$member_casual +trips_data_v2$day_of_week,FUN= mean)
##analyzing ridership data by type and weekday
trips_data_v2%>%
  mutate(weekday=wday(started_at,label = TRUE))%>%
  group_by(member_casual,weekday)%>%
  summarise(number_of_rides=n)%>%
  mean(ride_time)
##let visualize the data 
trips_data_v2%>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, wmemreekday) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_time)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position ="dodge")

##exporting the data for better visualization in tableau
write.csv(trips_data_v2,"D:/DATA ANALYSIS/GOOGLE DATA ANALYTICS/cyclist_data/trips_data_v2.csv",row.names = FALSE)

                      
