#libraries
library(plyr)
library(caret)
library(readxl)
library(xlsx)
library(adabag)
library(DMwR)
library(gbm)



# change this path to your dataset path
datasetPath <- "C:/Users/Mahdi/Documents/R/Regression/Regression.xlsx"  

# sample dataset
data <- as.data.frame(read_excel(datasetPath))

lableCol <- 5    # The 5th column is the class column. In your dataset maybe different.




# split dataset to train and test
sample <- sample.int(n = nrow(data) , size = floor(0.75*nrow(data)) , replace = F)
train <- data[sample, ]
test <- data[-sample, ]





# gbm  parameters . you can change this parameters

distribute <- "gaussian"    
#gaussian   laplace   tdist   bernoulli 
#huberized  adaboost  poisson 
# coxph   quantile    pairwise

cvfolds <- 10   #default
ntrees <- 100  #default




#set.seed(123)

# gbm
gbm1 <- gbm(
  train$Response ~. ,  # in this line change Response to your Class column name .
  data=train[-lableCol] ,
  distribution = distribute,
  cv.folds = cvfolds ,
  n.trees = ntrees ) 



# find best iteration
best.iter <- gbm.perf(gbm1, method = "cv")
print(best.iter)
# best.iter <- gbm.perf(gbm1, method = "OOB")
# print(best.iter)
# 
# best.iter <- gbm.perf(gbm1, method = "test")
# print(best.iter)
# 



# predict
pred <- as.data.frame(predict(gbm1, newdata = test[-lableCol], n.trees = best.iter, type = "link"))
predResult <- cbind(pred , test$Response)     # in this line change Response to your Class column name .
names(predResult) <- c("Prediction" , "Actual")




# RMSE
rmse <- RMSE(predResult$Actual , predResult$Prediction)





# r-squre
mse <- rmse^2 
sse <- mse * nrow(test)
tss <- sum((test$Response - mean(test$Response))^2)
Rsqure <- 1 - (sse/tss)


sprintf('RMSE = %s , Rsqure = %s', rmse , Rsqure) 
