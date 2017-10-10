library(caret)
library(mlbench)

data<-BostonHousing
print(data)

dim(data)

indexes = createDataPartition(data,p=0.4,list = FALSE, times = 1) #randomly separating 40% of data by index

test_set<- data[indexes,] #assigninng 40% data to test
training<- data[-indexes,]  #assigning remaining 60% data to training set


