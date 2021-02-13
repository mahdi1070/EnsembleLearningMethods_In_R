#libraries
library(plyr)
library(randomForest)
library(caret)
library(readxl)
library(xlsx)
#install.packages('e1071', dependencies=TRUE)


# change this path to your dataset path
datasetPath <- "C:/Users/Mahdi/Documents/R/classification/Classification.xlsx"  

# sample dataset
data <- as.data.frame(read_excel(datasetPath))


lableCol <- 27    # The 27th column is the class column. In your dataset maybe different.




# in this dataset , train and test samples are specified
# So there is no need to split the data to train and test samples

data_test <- data[data$`Train/Test` == 'Test', ]
data_train <- data[data$`Train/Test` == 'Train', ]
data_test <- data_test[ -c(28) ]
data_train <- data_train[ -c(28) ]




# in classification dataset we must change class column type to factor type
data_test$Label = factor(data_test$Label)
data_train$Label = factor(data_train$Label)





# random forest  parameters . you can change this parameters
importa <- TRUE
proximi <- FALSE
ntrees <- 2500
nodeSize <- 1  #DEFAULT
Mtr = floor(sqrt(ncol(data_train[-lableCol])))

# res <- as.data.frame(tuneRF(data_train[-lableCol] , data_train$Label , stepFactor = .5 , ntreeTry = ntrees))
# Mtr <- res[match(c(min(res$OOBError)) , res$OOBError ),1]





# randomforest
rf <- randomForest(
  data_train$Label ~ . ,    # in this line change Label to your Class column name .
  data=data_train[-lableCol]  ,
  importance=importa ,
  ntree=ntrees ,
  mtry = Mtr ,
  nodeSize = nodeSize ,
  proximity = proximi ,
)




# predict
pred <- as.data.frame(predict(rf, type = "response" ,newdata=data_test[-lableCol]))
predResult <- cbind(pred , data_test$Label)    # in this line change Label to your Class column name .
names(predResult) <- c("Prediction" , "Actual")



# confusion Matrix
cn <- confusionMatrix(predResult$Prediction , predResult$Actual)



# totalError and  KappaError and accuracy
eval <- as.data.frame(cn$overall) ; 
totalError <- 1 - eval$`cn$overall`[1]
KappaError <- 1 - eval$`cn$overall`[2]
accuracy <- eval$`cn$overall`[1]



sprintf('accuracy = %s , totalError = %s , KappaError = %s', accuracy ,  totalError , KappaError) 

