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




names(getModelInfo())

fitControl<-trainControl(method = "repeatedcv",repeats = 8)


# RidgeModel <- list(label = "Ridge Regression",
#               library = "statPack",
#               type = "Regression",
#      parameters = data.frame(parameter = c('lambda'),
#                              class = c("numeric"),
#                              label = c('Weight Decay')),
#      grid = function(x, y, len = NULL, search = "grid")  {
#        if(search == "grid") {
#          out <- expand.grid(lambda = c(0, 10 ^ seq(-1, -4, length = len - 1)))
#        } else {
#          out <- data.frame(lambda = 10^runif(len, min = -5, 1))
#        }
#        out
#      },
#      loop = NULL,
#      fit = function(x, y, wts, param, lev, last, classProbs, ...) {
#
#        #creating df of predictors
#        dat <- as.data.frame(x)
#
#        respvector <- NULL
#        respname <- NULL
#        respnum <- NULL
#
#
#        for(i in 1:ncol(x)){
#          if(identical(y,dat[,i])){
#            respvector <- dat[,i]
#            respname <- names(x)[i]
#            respnum <- i
#          }
#        }
#
#        formula <- paste(respname,"~", sep="")
#
#        if(ncol(x) > 1){
#          for(i in 1:ncol(x)){
#            if(i != respnum){
#              formula <- paste(formula, "+", names(dat)[i], sep="")
#            }
#          }
#        }
#
#        formula <- as.formula(formula)
#        print(formula)
#
#       r<-statPack::ridgereg(formula, dat, lambda = param$lambda)
#
#       },
#      predict = function(modelFit, newdata, submodels = NULL) {
#        predict(modelFit, newdata,
#                s = 1,
#                mode = "fraction")$fit
#      },
#      predictors = function(x, s = NULL, ...) {
#        if(is.null(s))
#        {
#          if(!is.null(x$tuneValue))
#          {
#            s <- x$tuneValue$.fraction
#          } else stop("must supply a vaue of s")
#          out <- predict(x, s = s,
#                         type = "coefficients",
#                         mode = "fraction")$coefficients
#
#        } else {
#          out <- predict(x, s = s)$coefficients
#
#        }
#        names(out)[out != 0]
#      },
#      tags = c("Linear Regression", "L2 Regularization"),
#      prob = NULL,
#      sort = function(x) x[order(-x$lambda),])
#
#
#
#
#
# Ridge_fitting <- train(rm ~ ., data = training,
#                    method = RidgeModel,
#                    preProc = "lambda",
#                    tuneLength = 8,
#                    trControl = fitControl)
# Ridge_fitting
# ridgereg_own_fit <- caret::train(rm ~ ., data = training, method = "RidgeModel")





ridgemodel <- list(type = "Regression",
                   library = "statPack"
)

ridgemodel$parameters<-data.frame(parameter="lambda",
                                  class="numeric",
                                  label="lambda")

Fit<-function(x,y,lambda,param,lev,last,classProbs,...){

  dat <- as.data.frame(x)

  respvector <- NULL
  respname <- NULL
  respnum <- NULL

  for(i in 1:ncol(x)){
    if(identical(y,dat[,i])){
      respvector <- dat[,i]
      respname <- names(x)[i]
      respnum <- i
    }
  }

  formula <- paste(respname,"~", sep="")

  if(ncol(x) > 1){
    for(i in 1:ncol(x)){
      if(i != respnum){
        formula <- paste(formula, "+", names(dat)[i], sep="")
      }
    }
  }

  formula <- as.formula(formula)
  model <- ridgereg( formula = formula, data=dat,lambda= param$lambda)
  return(model)
}

ridgemodel$fit<-Fit

ridgemodel$predict<-function(modelFit, newdata, preProc = NULL, submodels = NULL){

  predict(modelFit,newdata)
}

ridgemodel$prob<- list(NULL)

ridgemodel$sort<-function (x) x[order(-x$lambda), ]

ridgemodel$label<-"Ridge Regression"

ridgemodel$grid<-function(x,y,len=NULL, search="grid"){
  data.frame(#lambda=seq(from=20, to=45, by=1))
    lambda=seq(from=0, to=200, by=10))
}

set.seed(-264619L)
ctrl <- trainControl(method = "repeatedcv",  number = 10)
ridgeFit <- caret::train( y = training$crim,
                          x = training,
                          method = ridgemodel,
                          # trControl = ctrl
)
