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
  combine_data <- inner_join(flights, airports, by = c("dest" =  "faa"))
  flights <- summarise(group_by(flights, dest), M = mean(arr_delay))
  
  # data_frame <- data.frame(flight_delay_mean,lat_lng[,2])
  # library(ggplot2)
  # p<- ggplot(data_frame, aes(x = dest, y = arr_delay_mean, label = cord)) + 
  #   geom_point() 
  # return(p)
  
}

# visualize_airport_delays()

