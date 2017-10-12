#library(statPack)
library(caret)
# library(lattice)
#require(ggplot2)
library(mlbench)


data("BostonHousing") #load a data
boston_data <- BostonHousing #set a data to variable
indexes = caret::createDataPartition(boston_data$rm, p = .70, list = FALSE, times = 1)
training<- boston_data[indexes,] #assigninng 70% data to test
testing<- boston_data[-indexes,]  #assigning remaining 30% data to training set

set.seed(12345)
ridgereg_fit <- caret::train(rm ~ . , data = training, method = "lm")
print(ridgereg_fit)


ridgereg_forward_fit <- caret::train(rm ~ ., data = training, method = "leapForward")
print(ridgereg_forward_fit)

# fitControl <- trainControl(## 10-fold CV
#   method = "repeatedcv",
#   number = 10,
#   ## repeated ten times
#   repeats = 10)


#fitting ridge regression model

# RidgeModel <- list(type = "Regression",library = "statPack",loop = NULL)  #to load package if needed
# 
# prm <- data.frame(parameter = "lambda",class = "numeric",label = "lambda")
# 
# RidgeModel$parameters <- prm
# 
# print(RideModel)

names(getModelInfo())

fitControl<-trainControl(method = "repeatedcv",repeats = 8)

RidgeModel <- list(label = "Ridge Regression",
                  library = "statPack",
                  type = "Regression",
                  parameters = data.frame(parameter = c('lambda'),
                                          class = c("numeric"),
                                          label = c('Weight Decay')),
                  grid = function(x, y, len = NULL, search = "grid")  {
                    if(search == "grid") {
                      out <- expand.grid(lambda = c(0, 10 ^ seq(-1, -4, length = len - 1)))
                    } else {
                      out <- data.frame(lambda = 10^runif(len, min = -5, 1))
                    }
                    out
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    statPack::ridgereg(as.matrix(x), y, lambda = param$lambda)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    predict(modelFit, newdata, 
                            s = 1, 
                            mode = "fraction")$fit
                  },
                  predictors = function(x, s = NULL, ...) {
                    if(is.null(s))
                    {
                      if(!is.null(x$tuneValue))
                      {
                        s <- x$tuneValue$.fraction
                      } else stop("must supply a vaue of s")
                      out <- predict(x, s = s,
                                     type = "coefficients",
                                     mode = "fraction")$coefficients
                      
                    } else {
                      out <- predict(x, s = s)$coefficients
                      
                    }
                    names(out)[out != 0]
                  },
                  tags = c("Linear Regression", "L2 Regularization"),
                  prob = NULL,
                  sort = function(x) x[order(-x$lambda),])


#RidgeModel$ridgefit()

Ridge_fitting <- train(rm ~ ., data = training, 
                   method = RidgeModel, 
                   preProc = "lambda",
                   tuneLength = 8,
                   trControl = fitControl)
Ridge_fitting
# ridgereg_own_fit <- caret::train(rm ~ ., data = training, method = "RidgeModel")
# print(ridgere)