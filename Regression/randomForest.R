#libraries
library(plyr)
library(randomForest)
library(caret)
library(readxl)
library(xlsx)
#install.packages('e1071', dependencies=TRUE)


# change this path to your dataset path
datasetPath <- "C:/Users/Mahdi/Documents/R/Regression/Regression.xlsx"  


# sample dataset
data <- as.data.frame(read_excel(datasetPath))


lableCol <- 5    # The 5th column is the class column. In your dataset maybe different.



# split dataset to train and test
sample <- sample.int(n = nrow(data) , size = floor(0.75*nrow(data)) , replace = F)
train <- data[sample, ]
test <- data[-sample, ]


# random forest  parameters . you can change this parameters
proximi <- FALSE
importa <- TRUE
ntrees <- 50
nodeSize <- 5  #DEFAULT
Mtr = floor(ncol(train[-lableCol]))  

# res <- as.data.frame(tuneRF(train[-lableCol] , train$Response,stepFactor = .5 , ntreeTry = ntrees))
# Mtr <- res[match(c(min(res$OOBError)) , res$OOBError ),1]





# randomforest
rf <- randomForest(
  train$Response ~ . ,    # in this line change Response to your Class column name .
  data= train[-lableCol]  ,
  importance=importa ,
  ntree=ntrees ,
  mtry = Mtr ,
  nodeSize = nodeSize ,
  proximity = proximi
)





# predict
pred <- as.data.frame(predict(rf, type = "response" ,newdata=test[-lableCol]))
predResult <- cbind(pred , test$Response)   # in this line change Response to your Class column name .
names(predResult) <- c("Prediction" , "Actual")




# RMSE
rmse <- RMSE(predResult$Actual , predResult$Prediction)




# r-squre
mse <- rmse^2 
sse <- mse * nrow(test)
tss <- sum((test$Response - mean(test$Response))^2)
Rsqure <- 1 - (sse/tss)


sprintf('RMSE = %s , Rsqure = %s', rmse , Rsqure) 

