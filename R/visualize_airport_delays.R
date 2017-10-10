#'@title Visualize ariport Flight delays
#'@name  visualize_airport_delays
#'@description In this you can see that which flight is delay at what is its mean by its lat lng
#'@field no argument
#'@export

visualize_airport_delays <- function() 
{
  library(nycflights13)
   
  library(dplyr)
  library(ggplot2)
  flights <- nycflights13::flights
  airports <- nycflights13::airports
  associatedData <- inner_join(flights,airports,by = c("dest" = "faa"))
  
  subsettedData <- dplyr::select(associatedData,dest,lat,lon,arr_delay)
  
  meansWithLatLon <- subsettedData %>%
    group_by(dest,lat,lon) %>%
    summarize(Mean = mean(arr_delay,na.rm = TRUE))
  
  p <- ggplot(meansWithLatLon, aes(x = dest, y = Mean, label = dest)) +
    geom_point() + labs(x = "Airports") + theme_bw()
  
  return(p)
  
  
}

# visualize_airport_delays()

