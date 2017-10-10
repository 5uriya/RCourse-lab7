library(caret)
library(mlbench)

data("BostonHousing")
Boston_data<-BostonHousing
print(data)

dim(data)

indexes = sample(1:nrow(Boston_data),size = 0.4*nrow(Boston_data)) #randomly separating 40% of data by index

test_set<- Boston_data[indexes,] #assigninng 40% data to test
training<- Boston_data[-indexes,]  #assigning remaining 60% data to training set

set.seed(12345)
Ridgereg_Fit <- train(Boston_data$rm ~ ., data = Boston_data, 
                 method = "lm",
                 )
RidgeregFit

# fitControl <- trainControl(## 10-fold CV
#   method = "repeatedcv",
#   number = 10,
#   ## repeated ten times
#   repeats = 10)