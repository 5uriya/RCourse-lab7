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

  
  #qyery to combine data
  # combine_data <- inner_join(airports, flights, by = c ("faa"= "dest"))
  # group_data <- combine_data %>%  group_by(dest)
  # flight_delay_mean <- group_data %>% summarise('arr_delay_mean' = mean( arr_delay))
  # 
  
  mean_data <- dplyr::summarise(dplyr::group_by(flights, dest), M = mean(arr_delay))
  combine_data <- dplyr::inner_join(airports,mean_data, by = c("faa" =  "dest"))
  
    ggplot2::ggplot(combine_data, ggplot2::aes(x=combine_data$lat, y=combine_data$lon)) +
    ggplot2::geom_point(ggplot2::aes(color=combine_data$M), size=3) +
    ggplot2::scale_color_gradient(low="black", high="#F5F5F5") +
    ggplot2::theme_bw() +
    ggplot2::labs(title="Average arrival delays",
                  subtitle="Longitude vs. Latitude",
                  x="Latitude", y="Longitude",
                  color="Arrival delays") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust=0.5, size=16),
                   plot.subtitle = ggplot2::element_text(hjust = 0.5, size=14, face="italic"),
                   axis.text = ggplot2::element_text(size=12))
  
  #ggplot(mean_delay, aes(combine_data$lat,combine_data$lon, fill = weight)) + geom_raster() + coord_fixed(ratio = 1)  + scale_fill_gradientn(colours = rev(rainbow(7))
  
  # data_frame <- data.frame(flight_delay_mean,lat_lng[,2])
  # library(ggplot2)
  # p<- ggplot(data_frame, aes(x = dest, y = arr_delay_mean, label = cord)) + 
  #   geom_point() 
  # return(p)
  
}

visualize_airport_delays()

