rm(list=ls())
setwd("J:/data scientist/edwisor/PROJECT2/ARPIT SANTANDER-CUSTOMER-TRANSACTION-PREDICTION-master")
getwd()


#Reading the data
train = read.csv("J:/data scientist/edwisor/PROJECT2/ARPIT SANTANDER-CUSTOMER-TRANSACTION-PREDICTION-master/train.csv", header = T)
test = read.csv("J:/data scientist/edwisor/PROJECT2/ARPIT SANTANDER-CUSTOMER-TRANSACTION-PREDICTION-master/test.csv", header = T)
head(test)
head(train)
summary(test)
summary(train)
data(test)
data(train)
#Explore the data
str(train)


#Missing Values Analysis
missing_val = data.frame(apply(train,2,function(x){sum(is.na(x))}))


#Outlier Analysis
#BoxPlots - Distribution and Outlier Check
cnames = colnames(train[,3:4])
#Remove outliers using boxplot method
df1 = train
train = df1
#loop to remove from all variables
cnames = colnames(train[,3:202])
for(i in colnames){
  print(i)
  val = train[,i][train[,i] %in% boxplot.stats(train[,i])$out]
  print(length(val))
  marketing_train = train[which(!train[,i] %in% val),]
}

#Feature Selection
#Correlation Plot
install.packages("corrgram")
library(corrgram)
corrgram(train[1:30,3:202], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#Feature Scaling
#Normality check
qqnorm(train$var_1)
cnames = colnames(train[,2:202])

for(i in cnames){
  print(i)
  train[,i] = (train[,i] - min(train[,i]))/
    (max(train[,i] - min(train[,i])))
}


#Model Development
rmExcept("train")

#Divide data into train and test using stratified sampling method
set.seed(12234)
install.packages('caret')
library(caret)
train.index = createDataPartition(train$target, p = .80, list = FALSE)
train = train[ train.index,]
test  = train[-train.index,]

#Decision tree for classification
install.packages('C50')
library(C50)
#Develop Model on training data
str(train$target)
train$target = as.factor(train$target)

Ctree_model = C5.0(target ~., trn, trials = 1, rules = TRUE)

#Summary of DT model
summary(Ctree_model)

#writing rules into disk
write(capture.output(summary(Ctree_model)), "c50Rules.txt")

#predicting for test cases
Ctree_Predictions = predict(Ctree_model, test[,-2], type = "class")

#Evaluating the performance of classification model
ConfMatrix_Ctree = table(test$target, Ctree_Predictions)
confusionMatrix(ConfMatrix_Ctree)

#False Negative rate
FNR = FN/FN+TP 

#Accuracy: 90.89%
#FNR: 63.09%



#Logistic Regression
lr_model = glm(target ~ ., data = trn, family = "binomial")

#summary of the model
summary(lr_model)

#predicting using logistic regression
lr_Predictions = predict(lr_model, newdata = test, type = "response")

#converting prob
lr_Predictions = ifelse(lr_Predictions > 0.5, 1, 0)


##Evaluating the performance of classification model
ConfMatrix_RF = table(test$target, lr_Predictions)

#False Negative rate
FNR = FN/FN+TP 

#Accuracy: 90.89
#FNR: 67.85




#naive Bayes
library(e1071)

#Develop model
NBay_model = naiveBayes(target ~ ., data = trn)

#predicting on test cases #raw
NBay_Predictions = predict(NBay_model, test[,3:202], type = 'class')


#Look at confusion matrix
Conf_matrix = table(observed = test[,2], predicted = NBay_Predictions)
confusionMatrix(Conf_matrix)

#Accuracy: 92.16
#Recall: 0.93 
#precision: 0.63
#specivity:0.70
2052/(2052+1169)
