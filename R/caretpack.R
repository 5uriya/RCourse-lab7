library(caret)
library(mlbench)

data("BostonHousing")
boston_data<-BostonHousing

# indexes = sample(1:nrow(boston_data),size = 0.4*nrow(boston_data)) #randomly separating 40% of data by index

indexes = createDataPartition(boston_data$rm, p = .75, list = FALSE, times = 1)


training<- boston_data[indexes,] #assigninng 75% data to test
testing<- boston_data[-indexes,]  #assigning remaining 25% data to training set

set.seed(12345)
ridgereg_fit <- train(rm ~ . , data = training, method = "lm")

set.seed(12345)
ridgereg_forward_fit <- train(rm ~ ., data = training, method = "leapForward")

#evalute leapForward and linear regiression 



# fitControl <- trainControl(## 10-fold CV
#   method = "repeatedcv",
#   number = 10,
#   ## repeated ten times
#   repeats = 10)


#fitting ridge regression model

Ridge <- list(type = "Regression",  #to load package if needed
              library = "statPack",
              loop = NULL) 

prm <- data.frame(parameter = "lambda",
                  class = "numeric",
                  label = "lambda")

Ridge$parameters <- prm

ridgeFit <- function(x, y, param) { 
  data <- as.data.frame(x)
  
}



