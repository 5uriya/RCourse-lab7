## ---- echo=FALSE , results= "hide"---------------------------------------
library(mlbench)
library(caret)
library(statPack)

## ------------------------------------------------------------------------

data("BostonHousing") #load a data
boston_data <- BostonHousing #set a data to variable
indexes = caret::createDataPartition(boston_data$rm, p = .70, list = FALSE, times = 1)
training<- boston_data[indexes,] #assigninng 70% data to test
testing<- boston_data[-indexes,]  #assigning remaining 30% data to training set

set.seed(12345)
ridgereg_fit <- caret::train(rm ~ . , data = training, method = "lm")
# summary(ridgereg_fit)
print(ridgereg_fit)
ridgereg_forward_fit <- caret::train(rm ~ ., data = training, method = "leapForward")
print(ridgereg_forward_fit)

## ------------------------------------------------------------------------
# set.seed(12345)
# ridgereg_fit <- train(rm ~ . , data = training, method = "lm")
# # summary(ridgereg_fit)
# print(ridgereg_fit)
# set.seed(12345)
# ridgereg_forward_fit <- train(rm ~ ., data = training, method = "leapForward")
# print(ridgereg_forward_fit)

