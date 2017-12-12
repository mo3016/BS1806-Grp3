#load data file
library("data.table")
library("dplyr")
setwd("C:/Users/Rejpal/Documents/Study/Imperial/Modules/On-going/Machine Learning/Assignments/Homework 3")
df <- fread("Loans_processed.csv")

str(df)

#convert to factors a dataframe
df <- df %>% mutate_if(is.character,as.factor)

#random df and split into training, validation and test data sets
set.seed(123)
df_rand <- df[sample(1:nrow(df)), ]

##QUESTION 1
#Split dataset into training, validation and test datasets
#training dataset
train <- df_rand[1:20000,1:8]

#validation dataset
val <- df_rand[20001:28000,1:8]

#test labels
test <- df_rand[28001:nrow(df),1:8]

#proportions check
prop.table(table(train$loan_status))
prop.table(table(val$loan_status))
prop.table(table(test$loan_status))


#--------------------------------------------------------------
#naive model
tab <- table(df_rand$loan_status)
Total <- tab[1] + tab[2]
x <- tab[1]/Total*100
default_naive_pred <- 100 - x
default_naive_pred
#--------------------------------------------------------------
##QUESTION 2
#C50 library
#install.packages("C50")
library(C50)

xtrain <- train[1:nrow(train),-8]
ytrain <- train[1:nrow(train),8]


#training model
train_model <- C5.0(xtrain, ytrain)
train_model
summary(train_model)

#boosting iterations using trials
train_model_iterations <- C5.0(xtrain, ytrain, trials = 100)
train_model_iterations
summary(train_model_iterations)

#predict using training model iterations
xval <- val[1:nrow(val),-8]

predict_val <- predict(train_model, xval)
predict_val
summary(predict_val)

table(predict_va)

#confusion matrix
library(gmodels)
CrossTable(x = val$loan_status, y = predict_val, prop.chisq = FALSE)

#----------------------------------------------------------------------------------
##QUESTION 3
#sensitivity matrix of approximately 25% - is therefore provided by the following
#cost matrix

cost.matrix <- matrix(c(0,5,15,0),2,2,byrow=TRUE)
rownames(cost.matrix) <- colnames(cost.matrix) <- c("Charged Off", "Fully Paid")
cost.matrix

##Run the model using cost.matrix on test model and then on validation
train_model <- C5.0(xtrain, ytrain, costs = cost.matrix)
summary(train_model)

cm1_predict_val <- predict(train_model, xval)
summary(cm1_predict_val)

CrossTable(x = val$loan_status, y = cm1_predict_val, prop.chisq = FALSE)

tab <- table(xval$loan_status,cm1_predict_val)
precision <- tab[1,1]/(tab[1,1]+tab[2,1]) 
precision

#---------------------------------------------------------------------------------
#sensitivity matrix of approximately 40% - is therefore provided by the following
#cost matrix

cost.matrix <- matrix(c(0,5,22,0),2,2,byrow=TRUE)
rownames(cost.matrix) <- colnames(cost.matrix) <- c("Charged Off", "Fully Paid")
cost.matrix

##Run the model using cost.matrix on test model and then on validation
train_model <- C5.0(xtrain, ytrain, costs = cost.matrix)
summary(train_model)

cm2_predict_val <- predict(train_model, xval)
summary(cm2_predict_val)

CrossTable(x = val$loan_status, y = cm2_predict_val, prop.chisq = FALSE)

tab <- table(xval$loan_status,cm2_predict_val)
precision <- tab[1,1]/(tab[1,1]+tab[2,1]) 
precision

#---------------------------------------------------------------------------------
#sensitivity matrix of approximately 50% - is therefore provided by the following
#cost matrix

cost.matrix <- matrix(c(0,5,25,0),2,2,byrow=TRUE)
rownames(cost.matrix) <- colnames(cost.matrix) <- c("Charged Off", "Fully Paid")
cost.matrix

##Run the model using cost.matrix on test model and then on validation
train_model <- C5.0(xtrain, ytrain, costs = cost.matrix)
summary(train_model)

cm3_predict_val <- predict(train_model, xval)
summary(cm2_predict_val)

CrossTable(x = val$loan_status, y = cm3_predict_val, prop.chisq = FALSE)

tab <- table(xval$loan_status,cm3_predict_val)
precision <- tab[1,1]/(tab[1,1]+tab[2,1]) 
precision

#-------------------------------------------------------------------------------------
##QUESTION 4
"Comparing the three models we get:
Sensitivity 25%
MR 25%
Acc 80.14%
FP = 75% vs. 85.8% actual

Sensitivity 40%
MR 29%
Acc 80.14%
FP = 75% vs. 85.8%

Sensitivity 50%
MR 33.52%
ACC 66.47%
FP 78.86% vs. 85.8%

Choose the model with the lowest MR which is sensitivity 25% and lowest FP.
"

#-------------------------------------------------------------------------------------
##QUESTION 5
"Run the model of sensitivity 25% on the test dataset"
cost.matrix <- matrix(c(0,5,15,0),2,2,byrow=TRUE)
rownames(cost.matrix) <- colnames(cost.matrix) <- c("Charged Off", "Fully Paid")
cost.matrix

##Run the model using cost.matrix on test model and then on test
train_model <- C5.0(xtrain, ytrain, costs = cost.matrix)

#test prediction
xtest <- test[1:nrow(test),-8]

test_predict <- predict(train_model, xtest)
summary(test_predict)

CrossTable(x = test$loan_status, y = test_predict, prop.chisq = FALSE)
tab <- table(test$loan_status,cm3_predict_val)
precision <- tab[1,1]/(tab[1,1]+tab[2,1]) 
precision











