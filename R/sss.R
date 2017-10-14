library(mlbench)
library(caret)
library(statPack)


data("BostonHousing") #load a data
boston_data <- BostonHousing #set a data to variable
indexes = createDataPartition(boston_data$crim, p = 0.8, list = FALSE, times = 1)
training<- boston_data[indexes,] 
testing<- boston_data[-indexes,]

#LM

set.seed(-312312L)
ridgereg_fit <- train(crim ~ . , data = training, method = "lm")
#LM leap forward
ridgereg_forward_fit <- train(crim ~ ., data = training, method = "leapForward")


#Custom Ridge


ridge <- list(type="Regression", 
              library="lab7",
              loop=NULL,
              prob=NULL)
ridge$parameters <- data.frame(parameter="lambda",
                               class="numeric",
                               label="lambda")
ridge$grid <- function(y,x, len=NULL, search="grid"){
  data.frame(lambda=c(0.1,0.5,1,2))
}
ridge$fit <- function (x, y, wts, param, lev, last, classProbs, ...) {
  dat <- if (is.data.frame(x)) x else as.data.frame(x)
  dat$.outcome <- y
  out <- ridgereg$new(.outcome ~ ., data = dat, lambda=param$lambda, ...)
  out
}
ridge$predict <- function (modelFit, newdata, submodels = NULL) {
  if (!is.data.frame(newdata)) 
    newdata <- as.data.frame(newdata)
  newdata[,apply(newdata, MARGIN=2, sd)!=0] <- scale(newdata[,apply(newdata, MARGIN=2, sd)!=0])
  modelFit$predict(newdata)
}


Fitcontrol <- trainControl(method = "repeatedcv",
                        number=10,
                        repeats = 10)


set.seed(12345)
(lm.ridge <- train(medv ~ ., data=training, method=ridge, trControl=Fitcontrol))