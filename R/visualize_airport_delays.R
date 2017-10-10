#'@title Visualize ariport Flight delays
#'@name  visualize_airport_delays
#'@description In this you can see that which flight is delay at what is its mean by its lat lng
#'@field no argument
#'@export

visualize_airport_delays <- function() 
{

  flights <- nycflights13::flights
  airports <- nycflights13::airports
  
  #qyery to combine data
  combine_data <- dplyr::left_join(flight, airport, by = c("dest" =  "faa"))
  
  
}

# visualize_airport_delays()

