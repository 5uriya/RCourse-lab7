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
  combine_data <- dplyr::left_join(flights, airports, by = c("dest" =  "faa"))
  #group_by data by dest
  group_data <- combine_data %>%  dplyr::group_by(dest)
  
}

# visualize_airport_delays()

