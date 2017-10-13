library(statPack)
library(caret)
# library(lattice)
#require(ggplot2)
library(mlbench)


data("BostonHousing") #load a data
boston_data <- BostonHousing #set a data to variable
indexes = caret::createDataPartition(boston_data$crim, p = .75, list = FALSE, times = 1)
training<- boston_data[indexes,] #assigninng 70% data to test
testing<- boston_data[-indexes,]  #assigning remaining 30% data to training set

ridge_custom <- list(type=c("Regression"),library="statPack",loop=NULL,prob=NULL)

ridge_custom$parameters <- data.frame(parameter="lambda",class="numeric",label="lambda")

ridge_custom$grid <- function(y,x, len=NULL, search="grid")
{
    if(search == "grid") {
    out <- expand.grid(lambda = c(0, 10 ^ seq(-1, -4, length = len - 1)))
  } else {
    out <- data.frame(lambda = 10^runif(len, min = -5, 1))
  }
  out


}

ridge_custom$fit <- function (x, y, wts, param, lev, last, classProbs, ...)
{
if (is.data.frame(x))
    dat<- x
  else as.data.frame(x)
    dat$.outcome <- y
  result <- ridgereg$new(dat$.outcome ~ ., data = dat, lambda=param$lambda, ...)
  result
}

ridge_custom$predict <- function (modelFit, newdata, submodels = NULL) {
  if (!is.data.frame(newdata))
  {
    newdata <- as.data.frame(newdata)
  }
  modelFit$predict(newdata)
}




# fitControl <- trainControl(
#   method = "repeatedcv",
#   number = 10,
#   repeats = 10)

set.seed(3245)
result <- train( medv ~ ., data=training, method="ridge")
result
