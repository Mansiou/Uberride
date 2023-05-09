library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(shiny)
library(tidyverse)
library(reshape2)
library(leaflet)
library(caret)

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

# separate the datetime column into date and time components
combined_UberRides <- separate(combined_UberRides, Date.Time, into = c("Date", "Time"), sep = "\\s+(?=[^\\s]+$)")


# Convert the Date column to a date format
combined_UberRides$Date <- as.Date(combined_UberRides$Date, format = "%m/%d/%y")

# Add columns for month and day
combined_UberRides <- mutate(combined_UberRides,
                             Month = month(Date, label = TRUE),
                             Day = day(Date))

#Separating the time in hour and minutes
combined_UberRides <- separate(combined_UberRides, Time, into = c("hour", "minute", "seconds"), sep = ":")

#A new column for day of the week using the wday() function
combined_UberRides <- combined_UberRides %>%
  mutate(day = wday(Date, label = TRUE))

#pivot table to display trips by hour 
trips_hour <- combined_UberRides%>%
  group_by(hour,Month)
trips_hour <- trips_hour %>%
  summarize(num_trips = n())
trips_pivot <- trips_hour %>%
  pivot_wider(names_from = hour, values_from = num_trips, values_fill = 0)

#Chart that shows Trips by Hour and Month
trips_hour_month <- combined_UberRides %>%
  group_by(hour, Month)
trips_hour_month <- trips_hour_month %>%
  summarize(num_trips = n())

ggplot(trips_hour_month, aes(x = hour, y = num_trips, fill = "month")) +
  geom_col(position = "dodge") +
  labs(x = "Hour of Day", y = "Number of Trips", fill = "Month") +
  scale_fill_discrete(name = "Month", labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

#Chart that displays Trips Every Hour.
ggplot(trips_hour, aes(x = hour, y = num_trips)) +
  geom_line() +
  labs(x = "Hour of Day", y = "Number of Trips")

#Plot data by trips taken during every day of the month.
trips_per_day <- combined_UberRides %>%
  group_by(Day) %>%
  summarize(num_trips = n())
ggplot(trips_per_day, aes(x = Day, y = num_trips)) +
  geom_line() +
  labs(x = "Day of Month", y = "Number of Trips")

# table that shows Trips Every Day (Max 31 days in a month so I should see total trips taken each day). 
trips_per_day <- combined_UberRides %>%
  group_by(day,Month)
trips_per_day <- trips_per_day %>%
  summarize(num_trips = n())
trips_per_day <- trips_per_day %>%
  arrange(Month)
View(trips_per_day)
write.csv(trips_per_day, "trips_per_day.csv", row.names = FALSE)

#Chart by Trips by Day and Month 
trips_per_day_month <- combined_UberRides %>%
   mutate(month=format(Date,"%b")) %>%
   group_by(month,day)
   summarize(num_trips = n()) %>%
  #group_by(Month, ,wday(Date, label = TRUE)) %>%
   summarize(total_trips = sum(num_trips))
ggplot(trips_per_day_month, aes(x = month, y = total_trips, fill = wday)) +
  geom_bar(stat = "identity") +
  labs(x = "Month", y = "Number of Trips", fill = "Day of Week")

#Chart Trips by Bases and Month
trips_per_base_month <- combined_UberRides %>%
  mutate(month = format(Date, "%b")) %>%
  group_by(month, Base) %>%
  summarize(total_trips = n())
ggplot(trips_per_base_month, aes(x = Base, y = total_trips, fill = month)) +
  geom_bar(stat = "identity") +
  labs(x = "Base", y = "Number of Trips", fill = "Month")

#heatmap that displays by hour and day 
trips_per_hour_day <- combined_UberRides %>%
  group_by(hour, day) %>%
  summarize(total_trips = n())

trips_per_hour_day_wide <- dcast(trips_per_hour_day, hour ~ day, value.var = "total_trips")
ggplot(melt(trips_per_hour_day_wide, id.vars = "hour"), aes(x = variable, y = hour, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(x = "Day of Week", y = "Hour of Day", fill = "Number of Trips")

#Heat map by month and day
trips_per_month_day <- combined_UberRides %>%
  mutate(month = factor(format(Date, "%b"), levels = month.abb),
         day = factor(format(Date, "%d"))) %>%
  group_by(Month, day) %>%
  summarize(total_trips = n())

trips_per_month_day_wide <- dcast(trips_per_month_day, day ~ Month, value.var = "total_trips")

ggplot(melt(trips_per_month_day_wide, id.vars = "day"), aes(x = variable, y = day, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "black", high = "purple") +
  labs(x = "Month", y = "Day", fill = "Number of Trips")

#Heat map by month and week
trips_per_month_week <- combined_UberRides %>%
  mutate(month = factor(format(Date, "%b"), levels = month.abb),
         week = factor(floor_date(Date, unit = "week"), format = "%Y-%m-%d")) %>%
  group_by(month, week) %>%
  summarize(total_trips = n())

trips_per_month_week_wide <- dcast(trips_per_month_week, week ~ month, value.var = "total_trips")
ggplot(melt(trips_per_month_week_wide, id.vars = "week"), aes(x = variable, y = week, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(x = "Month", y = "Week", fill = "Number of Trips")
#heatmap basses and day of week 
trips_per_base_dow <- combined_UberRides %>%
  mutate(dow = factor(wday(Date, label = TRUE, abbr = FALSE))) %>%
  group_by(Base, dow) %>%
  summarize(total_trips = n())

trips_per_base_dow_wide <- dcast(trips_per_base_dow, Base ~ dow, value.var = "total_trips")
ggplot(melt(trips_per_base_dow_wide, id.vars = "Base"), aes(x = variable, y = Base, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "green") +
  labs(x = "Day of Week", y = "Base", fill = "Number of Trips")

#leaflet
ui <- fluidPage(
  titlePanel("Trips Map"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date_range", label = "Select Date Range:",
                     start = "2022-01-01", end = "2022-12-31")
    ),
    mainPanel(
      leafletOutput("map", width = "100%", height = "700px")
    )
  )
)
server <- function(input, output) {
  
  # Filter data based on date range input
  filtered_data <- reactive({
    combined_UberRides %>%
      filter(Date >= input$date_range[1] & Date <= input$date_range[2])
  })
  
  # Create Leaflet map
  output$map <- renderLeaflet({
    leaflet(data = filtered_data()) %>%
      addTiles() %>%
      addMarkers(lng = ~Lon, lat = ~Lat, popup = ~paste( trips_per_base_dow_wide, "<br>",Date, "<br>", Base))
  })
  
}
shinyApp(ui = ui, server = server)

#prediction model 
model <- combined_UberRides %>%
  group_by(hour, Month, day) %>%
  summarise(total_trips = n())
ggplot(model, aes(x = Month, y = total_trips, fill = day)) +
  geom_bar(stat = "identity") +
  labs(x = "Month", y = "Number of Trips", fill = "day")

#shiny app
# Define UI for Shiny app
ui <- fluidPage(
  # Create a sidebar with the input options
  sidebarLayout(
    sidebarPanel(
      radioButtons("graph_type", label = "Select a graph type:",
                   choices = c("trips by day and month", "trips by hour and month", "trips by base","Heatmap 1","Heatmap 2","Heatmap 3","prediction model","map"))
    ),
    # Create the main panel with the graph output
    mainPanel(
      plotOutput("graph")
    )
  )
)
# Define server for Shiny app
server <- function(input, output) {
  
  output$graph <- renderPlot({
    if(input$graph_type == "trips by day and month") {
      ggplot(trips_per_day_month, aes(x = month, y = total_trips, fill = wday)) +
        geom_bar(stat = "identity") +
        labs(x = "Month", y = "Number of Trips", fill = "Day of Week")
    }
  })
  
  
  output$graph <- renderPlot({
    if(input$graph_type == "trips by hour and month") {
      ggplot(trips_hour_month, aes(x = hour, y = num_trips, fill = "month")) +
        geom_col(position = "dodge") +
         labs(x = "Hour of Day", y = "Number of Trips", fill = "Month") +
          scale_fill_discrete(name = "Month", labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
      
    }
  })
  
  
  output$graph <- renderPlot({
    if(input$graph_type == "trips by base") {
      ggplot(trips_per_base_month, aes(x = Base, y = total_trips, fill = month)) +
        geom_bar(stat = "identity") +
        labs(x = "Base", y = "Number of Trips", fill = "Month")
    }
  })
  output$graph <- renderPlot({
    if(input$graph_type == "Heatmap 1") {
      ggplot(melt(trips_per_month_week_wide, id.vars = "week"), aes(x = variable, y = week, fill = value)) +
        geom_tile() +
        scale_fill_gradient(low = "white", high = "steelblue") +
        labs(x = "Month", y = "Week", fill = "Number of Trips")
    }
  })
  output$graph <- renderPlot({
    if(input$graph_type == "Heatmap 2") {
      
      ggplot(melt(trips_per_month_day_wide, id.vars = "day"), aes(x = variable, y = day, fill = value)) +
        geom_tile() +
        scale_fill_gradient(low = "black", high = "purple") +
        labs(x = "Month", y = "Day", fill = "Number of Trips")
      
    }
  })
  output$graph <- renderPlot({
    if(input$graph_type == "Heatmap 3") {
      ggplot(melt(trips_per_base_dow_wide, id.vars = "Base"), aes(x = variable, y = Base, fill = value)) +
        geom_tile() +
        scale_fill_gradient(low = "white", high = "green") +
        labs(x = "Day of Week", y = "Base", fill = "Number of Trips")
      
    }
  })
  output$graph <- renderPlot({
    if(input$graph_type == "Heatmap 4"){
      ggplot(melt(trips_per_hour_day_wide, id.vars = "hour"), aes(x = variable, y = hour, fill = value)) +
        geom_tile() +
        scale_fill_gradient(low = "white", high = "steelblue") +
        labs(x = "Day of Week", y = "Hour of Day", fill = "Number of Trips")
    }
  })
  output$graph <- renderPlot({
    if(input$graph_type == "prediction model"){
      ggplot(model, aes(x = Month, y = total_trips, fill = day)) +
        geom_bar(stat = "identity") +
        labs(x = "Month", y = "Number of Trips", fill = "day")
    }
  }) 
  
}

# Run the app
shinyApp(ui, server)


