#'@title Visualize ariport Flight delays
#'@description In this you can see that which flight is delay at what is its mean by its lat lng
#' 
#'@examples
#' visualize_airport_delays()
#' @export 

visualize_airport_delays <- function()
{
  requireNamespace("tidyverse")
  requireNamespace("nycflights13")
  requireNamespace("dplyr")
  
  data(flights)
  data(airports)
  
  flight <- flights
  airport <- airports
  
    # ggplot(data_frame,aes(y=mean,x=dest, labels="cordinates"))+geom_point(shape=1,size=3)+xlab(paste("Fitted values", phras, sep = "\n"))+ ylab("Residuals")+ggtitle("Residuals vs Fitted") +geom_hline(yintercept=0, linetype="dashed")+geom_smooth(span = 1.5,colour="red",method="loess",se=FALSE)+annotation_custom(img, xmin = 1.4, xmax = 2.4, ymin = 0.8, ymax = 1.4)+theme_liu()
}

