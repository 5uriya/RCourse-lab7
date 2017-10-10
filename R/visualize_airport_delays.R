#'@title Visualize ariport Flight delays
#'@description In this you can see that which flight is delay at what is its mean by its lat lng
#' 
#'@examples
#' visualize_airport_delays()
#' @export 

visualize_airport_delays <- function()
{
  requireNamespace("tidyverse")
  require(tidyr)
  require(nycflights13)
  library(ggplot2)
  
  data(flights)
  data(airports)
  
  flight <- flights
  airport <- airports
  
  #qyery to combine data
  combine_data <- dplyr::left_join(flight, airport, by = c("dest" =  "faa"))
  
  #group_by data by dest
  group_data <- combine_data %>%  dplyr::group_by(dest)
  
  #mean of arr_delay
  flight_delay_mean <- group_data %>% dplyr::summarise('arr_delay_mean' = mean( arr_delay, na.rm = TRUE)) 
  
  #get lat and lng 
  lat_lng <- group_data %>% dplyr::summarise("cord" = sprintf("lat= %s lon = %s" , lat[1], lon[1]), 'faa' = faa)

  
  data_frame <- data.frame(flight_delay_mean,lat_lng[,2])
  p<- ggplot(data_frame, aes(x = faa, y = arr_delay_mean, label = cord)) + 
    geom_point() 
  return(p)
  # ggplot(data_frame,aes(y=mean,x=dest, labels="cordinates"))+geom_point(shape=1,size=3)+xlab(paste("Fitted values", phras, sep = "\n"))+ ylab("Residuals")+ggtitle("Residuals vs Fitted") +geom_hline(yintercept=0, linetype="dashed")+geom_smooth(span = 1.5,colour="red",method="loess",se=FALSE)+annotation_custom(img, xmin = 1.4, xmax = 2.4, ymin = 0.8, ymax = 1.4)+theme_liu()
}

