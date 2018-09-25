#-----------------------------------------------------------------------------------
#Support Vector Machines - Pattern Recognition
#-----------------------------------------------------------------------------------
# 1. Business Understanding: 
#-----------------------------------------------------------------------------------
#pattern recognition is that of handwritten digit recognition. 
#-----------------------------------------------------------------------------------
# 2. Data Understanding: 
#-----------------------------------------------------------------------------------
# Train data includes-
# Number of Instances: 60000
# Number of Attributes: 785
# Test data includes-
# Number of Instances: 10000
# Number of Attributes: 785
#-----------------------------------------------------------------------------------
#3.Data Preparation: 
#-----------------------------------------------------------------------------------
#Loading Neccessary libraries
library(kernlab)
library(readr)
library(caret)
library(dplyr)
library(e1071)
#-----------------------------------------------------------------------------------

#Reading the data
train_Data<-read.csv("mnist_train.csv", header = FALSE)
test_data<-read.csv("mnist_test.csv", header = FALSE)

#Understanding the dimensions 
dim(train_Data) #60000 Observations 785 Variables

#Structure of the data
str(train_Data)

#Printing the top few rows
head(train_Data)

#Exploring the data
summary(train_Data)

#checking missing value
sapply(train_Data, function(x) sum(is.na(x)))


# checking for blank values "" in any of the fields of dataset
sapply(train_Data, function(x) length(which(x == ""))) #No Balank values

#Verfying the Test Data set is having the same structure of the train data set
#Dimensions of the test data set
dim((test_data)) #10000 785 Variables

#-----------------------------------------------------------------------------------
#4.Model Building
# 4.1 Vanilla dot
#-----------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------
#The given data set has 60k observations, so considering only 4k observations
#-----------------------------------------------------------------------------------

colnames(train_Data)[1] <- "label"
colnames(test_data)[1] <- "label"



#convert label variable to factor
train_Data$label<-as.factor(train_Data$label)
test_data$label<-as.factor(test_data$label)

#temporary dataframe for the label results
keeptrain<-as.data.frame(train_Data$label)
keeptest<-as.data.frame(test_data$label)

#null label variable in both datasets
train_Data$label<-NA
test_data$label<-NA

# Scaling the variables except label in training and test data set
train_Data<-as.data.frame(scale(train_Data))
train_Data[is.na(train_Data)]<- 0 #replace NA with 0
test_data<-as.data.frame(scale(test_data))
test_data[is.na(test_data)]<- 0

#add back label
train_Data$label<-keeptrain$`train_Data$label`
test_data$label<-keeptest$`test_data$label`


train_sample <- train_Data[1:3000,]
test_sample <- test_data[1:4000,]

#-----------------------------------------------------------------------------------
#Building the model
#-----------------------------------------------------------------------------------

set.seed(100)

# Model with C = 1
model_1<- ksvm(label ~ ., data = train_sample,scale = FALSE,C=1)

# Predicting the model results
predict_1 <-predict(model_1, test_data)

# Confusion Matrix - Finding accuracy, Sensitivity and specificity
confusionMatrix(predict_1, test_data$label)

# Accuracy    : 0.8830
# Sensitivity : 0.9609
# Specificity : 0.9919 

#--------------------------------------------------------------------
# 4.2 Linear model - SVM
#####################################################################

model_2<- ksvm(label ~ ., data = train_sample,scale = FALSE,C=10)

# Predicting the model results
predict_2 <-predict(model_2, test_data)

# Confusion Matrix - Finding accuracy, Sensitivity and specificity
confusionMatrix(predict_2, test_data$label)

# Accuracy    : 0.8934
# Sensitivity : 0.9860 (Class: 1)
# Specificity : 0.9930 (Class: 1)

#--------------------------------------------------------------------
# 5.1 SVM with vannilladot kernel
#####################################################################

model_linear_vanilla <- ksvm(label~ ., data = train_sample, scale = FALSE, kernel = "vanilladot")

# Predicting the model results
predict_3 <-predict(model_linear_vanilla, test_data)

# Confusion Matrix - Finding accuracy, Sensitivity and specificity
confusionMatrix(predict_3, test_data$label)

# Accuraccy   : 0.8636
# Sensitivity : 0.9790 (Class: 1)
# Specificity : 0.9883 (Class: 1)

#--------------------------------------------------------------------
# 5.2 SVM with rbfdot kernel
#####################################################################

model_rbf <- ksvm(label~ ., data = train_sample, scale = FALSE, kernel = "rbfdot")

# Predicting the model results
predict_4 <-predict(model_rbf, test_data)

# Confusion Matrix - Finding accuracy, Sensitivity and specificity
confusionMatrix(predict_4, test_data$label)

# Accuracy    : 0.8836
# Sensitivity : 0.9807 (Class: 1)
# Specificity : 0.9930 (Class: 1)

#--------------------------------------------------------------------
# 6.1 Hyperparameter tuning and Cross Validation  - Linear - SVM 
#####################################################################

# We will use the train function from caret package to perform crossvalidation

trainControl <- trainControl(method="cv", number=5)
# Number - Number of folds 
# Method - cross validation

metric <- "Accuracy"

# making a grid of C values. 
grid <- expand.grid(C=seq(1, 5, by=1))

# Performing 5-fold cross validation
fit.svm <- train(label~., data=train_sample, method="svmLinear", metric=metric,tuneGrid=grid, trControl=trainControl)

# Printing cross validation result
print(fit.svm)

# Accuraccy : 0.9026

# Plotting "fit.svm" results
plot(fit.svm)

# Valdiating the model after cross validation on test data

evaluate_linear_test<- predict(fit.svm, test_data)
confusionMatrix(evaluate_linear_test, test_data$label)

# Accuracy    - 0.8636
# Sensitivity - 0.9790
# Specificity - 0.9883
