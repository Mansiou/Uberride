# Uber Rdie🚕
For my project, I used the dataset of the uber redes to analyze the date and time of the rides to get information about the number of rides for each hour,month and day. The goal of this project was to analyse when when there was a higher demand for rides.
# Data dictionary📓
we used the information from tha dataset uber rides and the columns used for analyzation were:

1. Date.Time
2. Longitude
3. Latitude
4. Base
# Data cleaning 
I added the hour,day an dmonth colum in the combined dataset.
```
combined_UberRides <- mutate(combined_UberRides,
                             Month = month(Date, label = TRUE),
                             Day = day(Date))

```
# Shiny app
i made a shiny app that will display all the graphs.
```
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
```
```
  output$graph <- renderPlot({
    if(input$graph_type == "trips by day and month") {
      ggplot(trips_per_day_month, aes(x = month, y = total_trips, fill = wday)) +
        geom_bar(stat = "identity") +
        labs(x = "Month", y = "Number of Trips", fill = "Day of Week")
    }
```
