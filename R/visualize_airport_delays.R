#'@title Visualize ariport Flight delays
#'@name  visualize_airport_delays
#'@description In this you can see that which flight is delay at what is its mean by its lat lng
#'@field no argument
#'@export

visualize_airport_delays <- function() 
{

  library(dplyr)
  library(ggplot2)

  flight <- nycflights13::flights
  airport <- nycflights13::airports

  combine_data <- dplyr::left_join(flight, airport, by = c("dest" =  "faa"))
  
  #group_by data by dest
  group_data <- combine_data %>%  
    group_by(dest)
  
  #mean of arr_delay
  flight_delay_mean <- group_data %>% 
    summarise('arr_delay_mean' = mean( arr_delay, na.rm = TRUE)) 
  
  #get lat and lng 
  lat_lng <- group_data %>% 
    summarise("cord" = sprintf("lat= %s lon = %s" , lat[1], lon[1]))
  
  data_frame <- data.frame(flight_delay_mean,lat_lng[,2])
  
  plotted_graph <- ggplot2::ggplot(data_frame, aes(x = dest, y = arr_delay_mean, label = cord)) + 
    geom_point() 
  return(plotted_graph)
  
}

# visualize_airport_delays()

