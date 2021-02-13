
#libraries
library(plyr)
library(caret)
library(readxl)
library(xlsx)
library(adabag)
library(DMwR)




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





# boosting  parameters . you can change this parameters
booss  <- FALSE
mfinall <- 100   #default    
coefflearn <-"Zhu"   #  'Breiman'    'Freund'     'Zhu'






# boosting
adaboostModel <- boosting(
  Label ~. ,     # in this line change Label to your Class column name .
  data=data_train  ,    
  boos  = booss ,
  mfinal = mfinall ,
  coeflearn=coefflearn
)



#impor <- as.data.frame(adaboostModel$importance)
#importanceplot(adaboostModel)




# predict
pred <- predict.boosting(adaboostModel ,data_test[-lableCol])
evol.train <- errorevol(adaboostModel , data_train)
evol.test <- errorevol(adaboostModel , data_test)
plot.errorevol(evol.test,evol.train)
prdProb <- as.data.frame(pred$prob)
prdClass <- as.data.frame(pred$class)
predResult <- cbind(prdClass , data_test$Label)    # in this line change Label to your Class column name .
names(predResult) <- c("Prediction" , "Actual")
predResult$Prediction = factor(predResult$Prediction)




# confusion Matrix
cn <- confusionMatrix(predResult$Prediction , predResult$Actual)




# totalError and  KappaError and accuracy
eval <- as.data.frame(cn$overall) ; 
totalError <- 1 - eval$`cn$overall`[1]
KappaError <- 1 - eval$`cn$overall`[2]
accuracy <- eval$`cn$overall`[1]



sprintf('accuracy = %s , totalError = %s , KappaError = %s', accuracy ,  totalError , KappaError) 

