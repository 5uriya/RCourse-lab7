#'@title Visualize ariport Flight delays
#'@name  visualize_airport_delays
#'@description In this you can see that which flight is delay at what is its mean by its lat lng
#'@field no argument
#'@export

visualize_airport_delays <- function() 
{

  flights <- nycflights13::flights
  flights <- na.omit(flights)
  airports <- nycflights13::airports
  
  
  #group_by data by dest
  library(dplyr)
  library(ggplot2)
  
  
  #calculating mean of flight delays
  mean_data <- summarise(group_by(flights, dest), M = mean(arr_delay))
  
  #qyery to combine data
  combine_data <- inner_join(airports,mean_data, by = c("faa" =  "dest"))
  
    ggplot(combine_data, aes(x=combine_data$lat, y=combine_data$lon)) +
    geom_point(na.rm = TRUE) + theme_gray() +
    labs(title="Average Flight Delays",subtitle="Longitude vs. Latitude",
      x="Latitude", y="Longitude")+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))
}

visualize_airport_delays()

