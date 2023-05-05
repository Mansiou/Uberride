library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(shiny)
library(tidyverse)

#set working directory 
setwd("~/Desktop/DATA 332")

UberSept <- read.csv('uber-raw-data-sep14.csv')
UberApr <- read.csv('uber-raw-data-apr14.csv')
UberAug <- read.csv('uber-raw-data-aug14.csv')
UberJul <- read.csv('uber-raw-data-jul14.csv')
UberJun<- read.csv('uber-raw-data-jun14.csv')
UberMay <- read.csv('uber-raw-data-may14.csv')


#bind all the files 
combined_UberRides <-rbind(UberSept,UberApr,UberAug,UberJul,UberJun,UberMay)

#change date column to date schema
mydata <- combined_UberRides

# Identify the date column
date_column <- combined_UberRides$Date.Time

# Convert the date column to POSIXct format
combined_UberRides$Date.Time <- as.POSIXct(date_column, format="%Y-%m-%d %H:%M:%S", tz="GMT")

# View the updated data
head(mydata)


# separate date and time components
date <- as.Date(mydata$Date.Time)
mydata$hour <- format(date, "%M")

# print the results
date
# [1] "2023-05-01"




# separate the datetime column into date and time components
mydata <- separate(mydata, Date.Time, into = c("Date", "Time"), sep = "\\s+(?=[^\\s]+$)")


# Convert the Date column to a date format
mydata$Date <- as.Date(mydata$Date, format = "%m/%d/%y")

# Add columns for month and day
mydata <- mutate(mydata,
                        Month = month(Date, label = TRUE),
                        Day = day(Date))

#Separating the time in hour and minutes
mydata <- separate(mydata, Time, into = c("hour", "minute", "seconds"), sep = ":")

#A new column for day of the week using the wday() function
mydata <- mydata %>%
  mutate(day = wday(Date, label = TRUE))

#Group the data by month and day and count the number of occurrences
ridesNum <- mydata %>%
  group_by(Month, Day) %>%
  summarize(ridesNum = n())

#Count the number of rides each month
monthNum <- mydata %>%
  group_by(Month) %>%
  summarize(ridesNum = n())

#Save the pivot tables as CSV files
write.csv(ridesNum, "rides_per_month.csv", row.names = FALSE)
write.csv(monthNum, "rides_per_month.csv", row.names = FALSE)

#Count the number of trips each hour
trips_per_hour <- mydata %>%
  group_by(hour, Month) %>%
  summarize(num_rides = n()) %>%
  arrange(hour)

#Save the pivot table as a CSV file
write.csv(trips_per_hour, "trips_per_hour.csv", row.names = FALSE)

#Count the number of trips every day of all the months
Month_day <- mydata %>%
  group_by(Month, day) %>%
  summarize(Trips = n())

#Save the pivot table as a CSV file
write.csv(Month_day, "Month_day.csv", row.names = FALSE)

#Count the number of trips according to bases
Count <- mydata %>%
  group_by(Base, Month) %>%
  summarize(n = n())

Save the pivot table as a CSV file
write.csv(Count, "Count.csv", row.names = FALSE)

Count the number of trips for each base and day of the week
Base_week <- mydata %>%
  group_by(Base, day) %>%
  summarize(num_trips = n())

Save the pivot table as a CSV file
write.csv(Base_week, "Base_week.csv", row.names = FALSE)

# graph showing the number of rides per month
ggplot(num_month, aes(x = Month, y = ridesNum, fill = Month)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Uber Rides per Month",
       x = "Month",
       y = "Number of Rides") +
  theme_classic()

# graph showing the number of trips every hour of the day by month
ggplot(trips_per_hour, aes(x = reorder(hour, -ridesNum), y = ridesNum, fill = Month)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Uber Rides Per Hour",
       x = "Time of Day",
       y = "Number of Rides") +
  theme_classic()

# graph showing the number of trips every hour
ggplot(trips_per_hour, aes(x = reorder(hour, -ridesNum), y = ridesNum, fill = hour)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Uber Rides Per Hour",
       x = "Time of Day",
       y = "Number of Rides") +
  theme_classic()

# graph showing the number of trips every day for each month with the day of the week
ggplot(Month_day, aes(x = Month, y = Trips, fill = day)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Trips every month with day fo the week",
       x = "Month",
       y = "Number of Rides") +
  theme_minimal()



