## ---- echo=TRUE , results= "hide", warning=FALSE , message=FALSE---------
library(mlbench)
library(caret)
library(statPack)

## ------------------------------------------------------------------------
data("BostonHousing") #load a data
boston_data <- BostonHousing #set a data to variable
indexes = createDataPartition(boston_data$rm, p = .70, list = FALSE, times = 1)
training<- boston_data[indexes,] #assigninng 70% data to test
testing<- boston_data[-indexes,]  #assigning remaining 30% data to training set


## ------------------------------------------------------------------------
set.seed(12345)
ridgereg_fit <- train(rm ~ . , data = training, method = "lm")
print(ridgereg_fit)

ridgereg_forward_fit <- train(rm ~ ., data = training, method = "leapForward")
print(ridgereg_forward_fit)

## ------------------------------------------------------------------------
ridgemodel <- list(type= "Regression" ,library="statPack",loop=NULL,prob=NULL)

ridgemodel$parameters <- data.frame(parameter="lambda",class="numeric",label= "Ridge Regression")

ridgemodel$grid <- function(y,x, len=NULL, search="grid")
{
  if(search == "grid") {
    out <- expand.grid(lambda = c(0, 10 ^ seq(-1, -4, length = len - 1)))
  } else {
    out <- data.frame(lambda = 10^runif(len, min = -5, 1))
  }
  out


}

ridgemodel$fit <- function (x, y, wts, param, lev, last, classProbs, ...)
{
  dat <- if (is.data.frame(x))
     x
  else as.data.frame(x)
  dat$.outcome <- y
  result <- statPack::ridgereg$new(dat$.outcome ~ ., data = dat, lambda=param$lambda, ...)
  result
}

ridgemodel$predict <- function (modelFit, newdata, submodels = NULL) {
  if (!is.data.frame(newdata))
  {
    newdata <- as.data.frame(newdata)
  }
  modelFit$predict(newdata)
}

## ---- echo=TRUE , results= "hide", warning=FALSE , message=FALSE---------
#result <- train( rm ~ ., data=training, method= ridgemodel, trControl = fitControl)
#result

