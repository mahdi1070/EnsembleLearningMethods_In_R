#libraries
#library(plyr)
library(ipred)
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




# bagging  parameters . you can change this parameters
nbag <- 50
coobb <- FALSE




# bagging
bagModel <- bagging(
  train$Response ~ . ,    # in this line change Response to your Class column name .
  data=train[-lableCol]  ,
  coob = coobb ,
  nbagg = nbag ,
)




# predict
pred <- as.data.frame(predict(bagModel ,newdata=test[-lableCol]))
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
