library(caret)
library(mlbench)

data("BostonHousing")
boston_data<-BostonHousing
print(data)

dim(data)

indexes = sample(1:nrow(boston_data),size = 0.4*nrow(boston_data)) #randomly separating 40% of data by index

# #indexes = createDataPartition(boston_data$rm, p = .4, 
#                     list = FALSE, 
#                     times = 1)


test_set<- boston_data[indexes,] #assigninng 40% data to test
training<- boston_data[-indexes,]  #assigning remaining 60% data to training set

set.seed(12345)

#

ridgereg_fit <- train(rm ~ ., data = boston_data, 
                 method = "lm"
                 )

lm <- summary(ridgereg_fit)
lm$coefficients


ridgereg_forward_fit <- train(rm ~ ., data = boston_data, 
                      method = "leapForward")

lm_forward <- summary(ridgereg_forward_fit)
lm_forward$which


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



