rm(list=ls())
setwd("I:/INSOFE_Singh1/Insofe_Project")
Data<-read.csv("Final.csv", header = TRUE,sep = )
Data$X <- NULL
Data$Purchased_Date <- NULL
Data$Duedate <- NULL
Data$NoOfDaysDue <- NULL
Data$NoOfDaysDue <- as.numeric(Data$NoOfDaysDue)
Data$For_How_Many_Days <- NULL
Data$Category <- as.factor(Data$Category)
Data$Age <- as.numeric(Data$Age)
#Data$IdMedication <- NULL

str(Data)
summary(Data)
 

#Adding a new variable t for times series

Data$t<-0

for (i in 1:nrow(Data))  {
  if(i>1){
    
    if(Data[i,]$IdMedication==Data[i-1,]$IdMedication)
    {
      Data[i,]$t<-Data[i-1,]$t+1
      
    }
    else
    {
      Data[i,]$t<-1
    }
    
  }
  else
  {
    Data[i,]$t<-1
  }
}


table(Data$Category)


#library(caTools)
#split=sample.split(Data$IdMedication ,SplitRatio = 0.90)
#split<-data.frame(split)
train <- data.frame(NULL)
test <- data.frame(NULL)
glm_train <- data.frame(NULL)
glm_test <- data.frame(NULL)

#FreqData <- table(Data$IdMedication)
#FreqTrain <- table(train$IdMedication)
#FreqTest <- table(test$IdMedication)
#Datafreq <- data.frame(FreqData)
#TrainFreq <- data.frame(FreqTrain)
#Testfreq <- data.frame(FreqTest)

#library(dplyr)
#distinct1 <- distinct(Data)
#distinct2 <- distinct(train)
#distinct3 <- distinct(test)


# freq <- as.data.frame(table(test$Category))
# freq

tf1 <- table(Data$IdMedication)
tf1 <- data.frame(table(ID = Data$IdMedication))
tf1$Freq
names(tf1)[names(tf1) == 'Var1'] <- 'IdMedication'

library(plyr)
Data<-merge(Data, data.frame(table(IdMedication = Data$IdMedication)), by = c("IdMedication"))

#Data<-Data[Data$ActGPI %in% names(tf1)[tf1>1],]

for (i in 1:nrow(Data)){
  if(i!=8764 ){
    
    if((Data[i,]$IdMedication==Data[i+1,]$IdMedication) | (Data[i,]$Freq == 1))
    {
      
      train<-rbind(train,Data[i,])
      
    }
    else
    {
      test<-rbind(test,Data[i,])
    }
    
  }
  else
  {
    test<- rbind(test,Data[i,])
  }
}

##################### Building GLM model################### 
glm_train <- train
glm_test <- test
glm_train$IdMedication <- NULL
#glm_train$Freq <- NULL
glm_test$IdMedication <- NULL
table(glm_train$Category)
table(glm_test$Category)
#glm_test$Freq <- NULL
#train$t <- NULL
summary(glm_train)
str(glm_train)
#write.csv(train, "glmTrainData.csv")
#write.csv(test, "glmTestData.csv")

# nums <- sapply(Data, is.numeric)
# nums <- as.data.frame(nums)
# Data[, lapply(Data, is.numeric) == TRUE, with = FALSE]
# cor(nums)
LogReg1 <- glm(Category ~ActGPI+RouteOfAdmin + Age + Sex + PurchasedBy + 
                 Pharmacy + t + Freq, data = glm_train, family = binomial)

#LogReg1 <- glm(Category ~., data = glm_train, family = binomial("logit"), maxit=100)


#LogReg1<- glm( Category ~ RouteOfAdmin + ActGPI + State + QTY +Age + MailRetail +
 #                AmountPaid + Sex + PurchasedBy + Pharmacy + t,
  #                data=glm_train,family = binomial)
summary(LogReg1)

# Predicting on the train data
prob<-predict(LogReg1, type="response")

# Considering the threshold as 0.5
pred_class <- ifelse(prob > 0.5, 1, 0)
#write.csv(pred_class, "glm_train_predictions.csv")
table(glm_train$Category,pred_class)

# Generating the confusion metric on train data
conf.mat1 = table(glm_train$Category,pred_class)

# Calculating the accuracy of the model on train data
glm_accuracy = sum(diag(conf.mat1))/sum(conf.mat1)
glm_precision = conf.mat1[2,2]/sum(conf.mat1[,2])
glm_recall = conf.mat1[2,2]/sum(conf.mat1[2,])
glm_specificity = conf.mat1[1,1]/sum(conf.mat1[1,])

 #predicting on test data
#test$IdMedication <- NULL
#test$Freq <- NULL
fitted.results <- predict(LogReg1,glm_test,type="response")
pred_class2 <- ifelse(fitted.results>0.5,1,0)
conf.mat2 = table(glm_test$Category,pred_class2)

glm_accuracy_test = sum(diag(conf.mat2))/sum(conf.mat2)
glm_precision_test = conf.mat2[2,2]/sum(conf.mat2[,2])
glm_recall_test = conf.mat2[2,2]/sum(conf.mat2[2,])
glm_specificity_test = conf.mat2[1,1]/sum(conf.mat2[1,])

###Identifying Variance inflation factor
library(car)
vif(LogReg1)
barplot()
?barplot

# 2. Stepwise Regression
library(MASS)
Step1 <- stepAIC(LogReg1)

###Use ROCR curve to obtain, reasonable cutoff for probabilities and using that probability as a threshold to obtain best set of predictions  

##ROCR curves..Loading the required libraries 
library(ROCR) 
library(PROC)
library(ggplot2) 
# Predicting on the train data 
predicted <- predict(LogReg1,type="response") 
prob1 <- prediction(predicted, glm_train$Category) 

# Getting the true positive rate and false negative rate 
tprfpr <- performance(prob1, "tpr", "fpr") 
# Plotting the true positive rate and false negative rate based on the threshold value 
plot(tprfpr) 
str(tprfpr) 
# For different threshold values identifying the tpr and fpr 
cutoffs <- data.frame(cut=tprfpr@alpha.values[[1]], fpr=tprfpr@x.values[[1]],  
                      tpr=tprfpr@y.values[[1]]) 
# Sorting the data frame in the decreasing order based on tpr 
cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=TRUE),] 
head(subset(cutoffs, fpr < 0.2)) 

# Plotting the true positive rate and false negative rate based based on the cutoff       
# increasing from 0.1-1 
plot(tprfpr, colorize = TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7)) 
tpr <- unlist(slot(tprfpr, "y.values")) 
fpr <- unlist(slot(tprfpr, "x.values")) 
# creating the data frame with tpr and fpr 
roc <- data.frame(tpr, fpr) 
# Plotting the graph 
ggplot(roc) + geom_line(aes(x = fpr, y = tpr)) +  
  geom_abline(intercept=0,slope=1,colour="gray") +  
  ylab("Sensitivity") +    xlab("1 - Specificity") 

#Using the ROC curve, obtain the appropriate threshold of probalility for  
## for calculating the error metric 
# Considering the threshold as 0.11 based on ROC curve 
Roc_pred_class1 <- ifelse(prob> 0.70, 1, 0) 
table(glm_train$Category,Roc_pred_class1) 
# Generate the confusion metrics on train data 
conf.mat1 = table(glm_train$Category,Roc_pred_class1) 
# Calculating the accuracy, precision and recall on train data 
glm_Roc_accuracy1 = sum(diag(conf.mat1))/sum(conf.mat1) 
glm_Roc_precision1 = conf.mat1[2,2]/sum(conf.mat1[,2]) 
glm_Roc_recall1 = conf.mat1[2,2]/sum(conf.mat1[2,]) 
glm_Roc_Specificity = conf.mat1[1,1]/sum(conf.mat1[1,])

# Test results  
fitted.results1 <- predict(LogReg1,glm_test,type='response') 
fitted.class1 <- ifelse(fitted.results1 > 0.70,1,0) 
# Generate the confusion metrics on test data 
conf.mat2 = table(glm_test$Category,fitted.class1) 
# Calculating the accuracy, precision and recall on test data 
glm_Roc_accuracyTest = sum(diag(conf.mat2))/sum(conf.mat2) 
glm_Roc_precisionTest = conf.mat2[2,2]/sum(conf.mat2[,2]) 
glm_Roc_recallTest = conf.mat2[2,2]/sum(conf.mat2[2,]) 
glm_Roc_SpecificityTest = conf.mat2[1,1]/sum(conf.mat2[1,])
#################Decision Trees#################
library(C50)
c50_Train <- glm_train
c50_Test <- glm_test
DT_C50 <- C5.0(Category ~ RouteOfAdmin +ActGPI+State+QTY+MailRetail+AmountPaid +Age + 
                 Sex + PurchasedBy + Pharmacy + t + Freq,
                 data=c50_Train)
plot(DT_C50)
#Predicting on train and test
c50_pred_Train = predict(DT_C50,newdata=c50_Train, type="class")
table(c50_pred_Train)
#write.csv(c50_pred_Train, "C50TrainPredictions")

c50_pred_Test = predict(DT_C50, newdata=c50_Test, type="class")
table(c50_pred_Test) 
#write.csv(c50_pred_Test,"C50TestPredictions")

# Generating the confusion metric on train data
C50.conf.mat1 = table(c50_Train$Category,c50_pred_Train)

# Calculating the accuracy of the model on train data
C50_accuracy_train = sum(diag(C50.conf.mat1))/sum(C50.conf.mat1)

# Calculating the precision of the model
C50_precision_train = C50.conf.mat1[2,2]/sum(C50.conf.mat1[,2])

# Calculating the recall of the model
C50_recall_train = C50.conf.mat1[2,2]/sum(C50.conf.mat1[2,])
C50_Specificity_train <- C50.conf.mat1[1,1]/sum(C50.conf.mat1[1,])

# Generating the confusion metric on test data
C50.conf.mat2 = table(c50_Test$Category,c50_pred_Test)

# Calculating the accuracy of the model on train data
C50_accuracy_test = sum(diag(C50.conf.mat2))/sum(C50.conf.mat2)

# Calculating the precision of the model
C50_precision_test = C50.conf.mat2[2,2]/sum(C50.conf.mat2[,2])

# Calculating the recall of the model
C50_recall_test = C50.conf.mat2[2,2]/sum(C50.conf.mat2[2,])
C50_specificity_test = C50.conf.mat2[1,1]/sum(C50.conf.mat2[1,])

#######################RPART#############
library(rpart)
library(rpart.plot)

rf_train <- c50_Train
rf_test <- c50_Test

rpartModel<- rpart(Category~., data=rf_train, method="class")
rpartModel
plot(rpartModel)

rf_pred_Train = predict(rpartModel,newdata=rf_train, type="class")
table(rf_pred_Train)

rf_pred_Test = predict(rpartModel, newdata=rf_test, type="class")
table(rf_pred_Test) 

rf.confmatrix1 <- table(rf_train$Category,rf_pred_Train)
rf_accuracy_train = sum(diag(rf.confmatrix1))/sum(rf.confmatrix1)
rf_precision_train = rf.confmatrix1[2,2]/sum(rf.confmatrix1[,2])
rf_recall_train = rf.confmatrix1[2,2]/sum(rf.confmatrix1[2,])
rf_Specificity_train <- rf.confmatrix1[1,1]/sum(rf.confmatrix1[1,])

rf.confmatrix2 <- table(rf_test$Category,rf_pred_Test)
rf_accuracy_test = sum(diag(rf.confmatrix2))/sum(rf.confmatrix2)
rf_precision_test = rf.confmatrix2[2,2]/sum(rf.confmatrix2[,2])
rf_recall_test = rf.confmatrix2[2,2]/sum(rf.confmatrix2[2,])
rf_Specificity_test <- rf.confmatrix2[1,1]/sum(rf.confmatrix2[1,])

#############PLOTS###########
#  attach(Data)
# str(Data)
# str(train)
# 
# boxplot(ActGPI~Age, main="Age VS ActGPI",
#         xlab="Age", ylab="ActGPI")
# 
# plot(PatientID,ActGPI)
# 
# library(ggplot2)
###########SVM model###########
library(e1071)
library(dummies)
svm_data <- Data
str(svm_data)
svm_data$IdMedication <- as.numeric(svm_data$IdMedication)
IdMedication <- svm_data$IdMedication
Category <- as.factor(svm_data$Category)

svm_factors <- subset(svm_data,select = -c(ActGPI,QTY,Age,AmountPaid,t,Freq,IdMedication,Category))
svm_factors$Category <- NULL
svm_numeric <- subset(svm_data, select = -c(RouteOfAdmin,State,MailRetail,Sex,PurchasedBy,Pharmacy,Category,Freq,t,IdMedication))

svm_factors_dummy <- dummy.data.frame(svm_factors)
str(svm_factors_dummy)

## standardizing the data
library(vegan)
svm_numeric_standarze <- decostand(svm_numeric,"range")
svm_numeric_standarze$Freq <- svm_data$Freq
svm_numeric_standarze$t <- svm_data$t
svm_numeric_standarze$IdMedication <- svm_data$IdMedication
svm_dataForModel <- cbind(svm_numeric_standarze,svm_factors_dummy,Category)

#splitting to train and test

svm_train <- as.data.frame(NULL)
svm_test <- as.data.frame(NULL)

for (i in 1:nrow(svm_dataForModel)){
  if(i!=8764 ){
    
    if((svm_dataForModel[i,]$IdMedication==svm_dataForModel[i+1,]$IdMedication) | (svm_dataForModel[i,]$Freq == 1))
    {
      
      svm_train<-rbind(svm_train,svm_dataForModel[i,])
      
    }
    else
    {
      svm_test<-rbind(svm_test,svm_dataForModel[i,])
    }
    
  }
  else
  {
    svm_test<- rbind(svm_test,svm_dataForModel[i,])
  }
}



# Store the independent variables and target variable separately (for easy use)
svm_train$IdMedication <- NULL
#svm_train$Freq <- NULL
svm_test$IdMedication <- NULL
#svm_test$Freq <- NULL
x = subset (svm_train, select = -Category) #remove response variable
y = as.factor(svm_train$Category)

str(svm_train)


#Build the model on train data

model = svm(x,y, method = "C-classification", kernel = "polynomial", cost = 10, gamma = 0.1)
summary(model)

svm_pred_class<-predict(model,type="response")
#svm_pred_class <- as.data.frame(svm_pred_class)
#table(svm_pred_class)

# Considering the threshold as 0.5
#svm_pred_class <- ifelse(svm_pred_class == 1, 1, 0)
#write.csv(svm_pred_class, "svmPredictionsTrain.csv")
table(svm_train$Category ,svm_pred_class)

# Generating the confusion metric on train data
svm_conf.mat1 = table(svm_train$Category,svm_pred_class)

# Calculating the accuracy of the model on train data
svm_accuracy1 = sum(diag(svm_conf.mat1))/sum(svm_conf.mat1)
svm_precision1 = svm_conf.mat1[2,2]/sum(svm_conf.mat1[,2])
svm_recall1 = svm_conf.mat1[2,2]/sum(svm_conf.mat1[2,])
svm_specificity_train = svm_conf.mat1[1,1]/sum(svm_conf.mat1[1,])

#predicting on test data
svm_test1=subset(svm_test,select=-Category)
svm_fitted.results <- predict(model,svm_test1)
#svm_pred_class2 <- ifelse(svm_fitted.results==1,1,0)
svm_conf.mat2 = table(svm_test$Category,svm_fitted.results)

# Calculating the accuracy of the model on test data
svmtest_accuracy2 = sum(diag(conf.mat2))/sum(conf.mat2)
svmtest_precision2 = conf.mat2[2,2]/sum(conf.mat2[,2])
svmtest_recall2 = conf.mat2[2,2]/sum(conf.mat2[2,])
svm_specificity_test = svm_conf.mat2[1,1]/sum(svm_conf.mat2[1,])

#######################NEURAL NETWORK######################################
# 
# install.packages("drat", repos="https://cran.rstudio.com")  #drat:::addRepo("dmlc")  #install.packages("mxnet") 
# require(mxnet)
# 
# nn_data <- Data
# nn_data$IdMedication <- as.numeric(nn_data$IdMedication)
# nn_data$Category <- as.numeric(nn_data$Category)
# str(nn_data)
# 
# library(dummies)
# nn_data <- dummy.data.frame(nn_data)
# 
# nn_train <- as.data.frame(NULL)
# nn_test <- as.data.frame(NULL)
# 
# for (i in 1:nrow(nn_data)){
#   if(i!=8764 ){
#     
#     if((nn_data[i,]$IdMedication==nn_data[i+1,]$IdMedication) | (nn_data[i,]$Freq == 1))
#     {
#       
#       nn_train<-rbind(nn_train,nn_data[i,])
#       
#     }
#     else
#     {
#       nn_test<-rbind(nn_test,nn_data[i,])
#     }
#     
#   }
#   else
#   {
#     nn_test<- rbind(nn_test,nn_data[i,])
#   }
# }
# 
# nn_train.x <- data.matrix(nn_data [nn_train, -74])
# library (caret)
# set.seed (1234)
# intrain = createDataPartition (y = nn_data$Category, p=0.7, list = F) 
# 
# train.x = data.matrix (nn_data [intrain, -74])
# train.y = nn_data [intrain, 74]
# test.x = data.matrix (nn_data [-intrain, 74]) 
# test.y = nn_data [-intrain, 74]
# 
# 
# nn_model<-mx.mlp(train.x, train.y, hidden_node =c(128,64), out_node=10, dropout = NULL, activation = "tanh", out_activation = "softmax",num.round=10, array.batch.size=100, learning.rate=0.07,  
#                momentum=0.9, eval.metric=mx.metric.accuracy)
# 
# # nn_model <- mx.mlp (train.x, test.x, hidden_node=10, out_node=1,activation="tanh",out_activation="rmse", num.round=20,array.batch.size=100, learning.rate=0.07, momentum=0.9, 
# #                  eval.metric=mx.metric.rmse) 
# 
# 
# nn_preds = predict(nn_model, test.x)
# 
# nn_preds=t(nn_preds)
# nn_pred.label = ifelse(nn_preds<0.55, 0, 1)
# 
# conf.mat = table(pred.label, test.y);conf.mat
# accuracy = sum(diag(conf.mat))/sum(conf.mat);accuracy
# precision = conf.mat[2,2]/sum(conf.mat[2,]);precision
# recall = conf.mat[2,2]/sum(conf.mat[,2]);recall
# 
# table(test.y)


########################KNN################################
knn_data <- Data
str(knn_data)
knn_data$IdMedication <- as.numeric(knn_data$IdMedication)
knn_data$Category <- as.numeric(knn_data$Category)

knn_data <- dummy.data.frame(knn_data)

knn_train <- as.data.frame(NULL)
knn_test <- as.data.frame(NULL)
knn_data$Category <- as.factor(knn_data$Category)

str(knn_data)
for (i in 1:nrow(knn_data)){
  if(i!=8764 ){
    
    if((knn_data[i,]$IdMedication==knn_data[i+1,]$IdMedication) | (knn_data[i,]$Freq == 1))
    {
      
      knn_train<-rbind(knn_train,knn_data[i,])
      
    }
    else
    {
      knn_test<-rbind(knn_test,knn_data[i,])
    }
    
  }
  else
  {
    knn_test<- rbind(knn_test,knn_data[i,])
  }
}

knn_train$IdMedication <- NULL
#knn_train$Freq <- NULL
knn_test$IdMedication <- NULL
#knn_test$Freq <- NULL

knn_trainWithoutLable <- subset(knn_train, select = -c(Category))
knn_testWithoutLable <- subset(knn_test, select = -c(Category))


library(class)
library(dummies)
library(vegan)


# N = 1/3/5/7
noOfNeigh <- 1 
knn_pred=knn(knn_trainWithoutLable, knn_testWithoutLable, knn_train$Category, k = noOfNeigh)
a1=table(knn_pred,knn_test$Category)
a1
knn1_accu= sum(diag(a1))/nrow(knn_testWithoutLable)
knn1_accu
knn1_precision <- a1[2,2]/sum(a1[,2])
knn1_recall <- a1[2,2]/sum(a1[2,])
knn1_specificity = a1[1,1]/sum(a1[1,])


noOfNeigh <- 3
knn_pred3=knn(knn_trainWithoutLable, knn_testWithoutLable, knn_train$Category, k = noOfNeigh)
a3=table(knn_pred3,knn_test$Category)
a3
knn3_accu= sum(diag(a3))/nrow(knn_testWithoutLable)
knn3_accu
knn3_precision <- a3[2,2]/sum(a3[,2])
knn3_recall <- a3[2,2]/sum(a3[2,])
knn3_specificity = a3[1,1]/sum(a3[1,])

noOfNeigh <- 5
knn_pred5=knn(knn_trainWithoutLable, knn_testWithoutLable, knn_train$Category, k = noOfNeigh)
a5=table(knn_pred5,knn_test$Category)
a5
knn5_accu= sum(diag(a5))/nrow(knn_testWithoutLable)
knn5_accu
knn5_precision <- a5[2,2]/sum(a5[,2])
knn5_recall <- a5[2,2]/sum(a5[2,])
knn5_specificity = a5[1,1]/sum(a5[1,])

# knn_accu= sum(diag(a))/nrow(knn_testWithoutLable)
# knn_accu
# svmtest_precision2 = conf.mat2[2,2]/sum(conf.mat2[,2])
# svmtest_recall2 = conf.mat2[2,2]/sum(conf.mat2[2,])
# svm_specificity_test = svm_conf.mat2[1,1]/sum(svm_conf.mat2[1,])

#Error metrices on test data
# a=table(knn_test$Category,knn_pred)
# a
# knn_accu <- sum(diag(a))/sum(a)
# knn_precision <- a[2,2]/sum(a[,2])
# knn_recall <- a[2,2]/sum(a[2,])
# knn_specificity = a[1,1]/sum(a[1,])

noOfNeigh <- 7
knn_pred7=knn(knn_trainWithoutLable, knn_testWithoutLable, knn_train$Category, k = noOfNeigh)
a7=table(knn_pred7,knn_test$Category)
a7
knn7_accu= sum(diag(a7))/nrow(knn_testWithoutLable)
knn7_accu
knn7_precision <- a7[2,2]/sum(a7[,2])
knn7_recall <- a7[2,2]/sum(a7[2,])
knn7_specificity = a7[1,1]/sum(a7[1,])


##############ENSEMBLE MODEL#################

# (4) Combining training predictions of CART, C5.0 & Log Regression together
#pred_class = glm train
#pred_class2 = glm train
#c50_pred_Train = c50 train
#c50_pred_Test = c5o test
#knn_pred = knn train
#a = knn test
#svm_pred_class = svm train
#svm_pred_class2 = svm test
# , knn_pred

#Combining training 
#svm_pred_class<-data.frame(svm_pred_class)
train_pred_all_models <- data.frame(cbind(Roc_pred_class1, c50_pred_Train,svm_pred_class))
#train_pred_all_models <- data.frame(apply(train_pred_all_models, 2, function(x) {as.factor(x)}))
train_pred_all_models$Roc_pred_class1 <- as.factor(train_pred_all_models$Roc_pred_class1)
# or first use "apply" then type data_ensemble <- data.frame(data_ensemble)
str(train_pred_all_models)
#train_pred_all_models$c50_pred_Train <- ifelse(train_pred_all_models$c50_pred_Train ==1,0,1)
#train_pred_all_models$c50_pred_Train<- as.factor(train_pred_all_models$c50_pred_Train)
summary(train_pred_all_models)
#rm(pred_class, pred_Train, svm_prob)

#  Viewing the predictions of each model
table(train_pred_all_models$Roc_pred_class1) #Logistic Regression
table(train_pred_all_models$c50_pred_Train) #C5.0
table(train_pred_all_models$svm_pred_class) #svm
table(train$Category) #Original Dataset lable

# Adding the original lable to the dataframe
train_pred_all_models <- cbind(train_pred_all_models, train$Category)
str(train_pred_all_models)
names(train_pred_all_models)[4] = "target"

# Ensemble Model with GLM as Meta Learner
str(train_pred_all_models)
head(train_pred_all_models)

glm_ensemble <- glm(target ~ ., train_pred_all_models, family = binomial())
summary(glm_ensemble)
str(glm_ensemble)

# Check the "glm_ensemble model" on the train data
pred_glm_ensemble <- predict(object = glm_ensemble, train_pred_all_models, type = "response")
pred_glm_ensemble <- ifelse(test = pred_glm_ensemble > 0.5, 1, 0)
table(pred_glm_ensemble)

confMatrix_ensemble <- table(train_pred_all_models$target,pred_glm_ensemble)
ensemble_accuracy_train = sum(diag(confMatrix_ensemble))/sum(confMatrix_ensemble)
ensemble_precision_train <- confMatrix_ensemble[2,2]/sum(confMatrix_ensemble[,2])
ensemble_recall_en_train = sum(confMatrix_ensemble[2,2])/sum(confMatrix_ensemble[2,]);
ensemble_specificity_train <- confMatrix_ensemble[1,1]/sum(confMatrix_ensemble[1,])


#Combining test predictions of test together 
svm_fitted.results <- data.frame(svm_fitted.results)
test_pred_all_models <- cbind(fitted.class1, c50_pred_Test, svm_fitted.results) 
test_pred_all_models$fitted.class1 <- as.factor(test_pred_all_models$fitted.class1)
test_data_ensemble <- data.frame(test_pred_all_models)
str(test_data_ensemble)
head(test_pred_all_models)

# converting all attributes to factors
#test_data_ensemble <- data.frame(apply(test_data_ensemble, 2, function(x) {as.factor(x)}))
#test_pred_all_models <- data.frame(apply(test_pred_all_models, 2, function(x) {as.factor(x)}))

# Change column names pred_class, pred_Train, svm_prob
#colnames(test_pred_all_models)[1:3] <- c("pred_class", "c50_pred_Train", "svm_pred_class")

str(test_pred_all_models)
test_pred_all_models <- as.data.frame(test_pred_all_models)


# test_pred_all_models$c50_pred_Test <- ifelse(test_pred_all_models$c50_pred_Test ==1,0,1)
# test_pred_all_models$svm_pred_class2 <- ifelse(test_pred_all_models$svm_pred_class2 ==1,0,1)
# test_pred_all_models$pred_class2 <- ifelse(test_pred_all_models$pred_class2 ==1,0,1)

# test_pred_all_models$pred_class2 <- as.factor(test_pred_all_models$pred_class2)
# test_pred_all_models$svm_pred_class2 <- as.factor(test_pred_all_models$svm_pred_class2)
# test_pred_all_models$c50_pred_Test<- as.factor(test_pred_all_models$c50_pred_Test)

#test_pred_all_models <-as.data.frame(test_pred_all_models)
# str(test_pred_all_models)
# str(c50_pred_Train)
# table(c50_pred_Train)
# table(pred_class)
# table(svm_pred_class)
test_pred_all_models <- cbind(test_pred_all_models, test$Category)
names(test_pred_all_models)[4] = "target"
# fitted.class1, c50_pred_Test, svm_fitted.results
# Roc_pred_class1, c50_pred_Train,svm_pred_class

test_pred_all_models$Roc_pred_class1<-test_pred_all_models$fitted.class1
test_pred_all_models$fitted.class1<-NULL
test_pred_all_models$c50_pred_Train<-test_pred_all_models$c50_pred_Test
test_pred_all_models$c50_pred_Test<-NULL
test_pred_all_models$svm_pred_class<-test_pred_all_models$svm_fitted.results
test_pred_all_models$svm_fitted.results<-NULL

# Check the "glm_ensemble model" on the test data
final_pred <- predict(glm_ensemble, test_pred_all_models, type = "response")
str(glm_ensemble)
str(test_pred_all_models)
#test_pred_all_models$c50_pred_Train<-as.factor(test_pred_all_models$c)
final_pred <- ifelse(test = final_pred > 0.5, 1, 0)
table(final_pred)

confMatrix_ensemble_test <- table(test_pred_all_models$target,final_pred)
ensemble_accuracy_test = sum(diag(confMatrix_ensemble_test))/sum(confMatrix_ensemble_test)
ensemble_precision_test <- confMatrix_ensemble_test[2,2]/sum(confMatrix_ensemble_test[,2])
ensemble_recall_test = sum(confMatrix_ensemble_test[2,2])/sum(confMatrix_ensemble_test[2,]);
ensemble_specificity_test <- confMatrix_ensemble_test[1,1]/sum(confMatrix_ensemble_test[1,])


############NAIVE BAYES################
nb_train <- glm_train
nb_test <- glm_test
nb_train$IdMedication <- NULL
nb_test$IdMedication <- NULL

library(e1071) 
nb_model = naiveBayes(Category ~ ., data = nb_train) 
summary(nb_model)

#Predicting on train
nb_pred = predict(nb_model, nb_train)
nb_conf_Matrix = table(nb_train$Category,nb_pred)


#Error Metrics on train
nb_accuracy_train = sum(diag(nb_conf_Matrix))/sum(nb_conf_Matrix)
nb_precision_train = nb_conf_Matrix[2,2]/sum(nb_conf_Matrix[,2])
nb_recall_Train = nb_conf_Matrix[2,2]/sum(nb_conf_Matrix[2,])
nb_specificity_train <- nb_conf_Matrix[1,1]/sum(nb_conf_Matrix[1,])

#Error Metrics on test
nb_pred2 = predict(nb_model, nb_test)
nb_conf_Matrix2 =table(nb_test$Category,nb_pred2)

#Error Metrics
nb_accuracy_test = sum(diag(nb_conf_Matrix2))/sum(nb_conf_Matrix2)
nb_precision_test = nb_conf_Matrix2[2,2]/sum(nb_conf_Matrix2[,2])
nb_recall_Test = nb_conf_Matrix2[2,2]/sum(nb_conf_Matrix2[2,])
nb_specificity_test <- nb_conf_Matrix2[1,1]/sum(nb_conf_Matrix2[1,])

###############RANDOM FORESTS###############
library(randomForest)
forest_train <- data.frame(NULL)
forest_test <- data.frame(NULL)
str(forest_train)

for (i in 1:nrow(Data)){
  if(i!=8764 ){
    
    if((Data[i,]$IdMedication==Data[i+1,]$IdMedication) | (Data[i,]$Freq == 1))
    {
      
      forest_train<-rbind(forest_train,Data[i,])
      
    }
    else
    {
      forest_test<-rbind(forest_test,Data[i,])
    }
    
  }
  else
  {
    forest_test<- rbind(forest_test,Data[i,])
  }
}

str(forest_train)
str(forest_test)

forest_train$IdMedication <- NULL
forest_test$IdMedication <- NULL
forest_Model <- randomForest(Category ~ ., data=forest_train, ntree=30,mtry = 4) 
summary(forest_Model)
print(forest_Model)
forest_Model$predicted
forest_Model$importance

#Plot() directly prints the important attributes
plot(forest_Model)
varImpPlot(forest_Model) 

#Predicting on train and test
forest_pred_train = predict(forest_Model,newdata=forest_train, type="class")
table(forest_pred_train)

forest_pred_test = predict(forest_Model, newdata=forest_test, type="class")
table(forest_pred_test) 
forest.conf.mat1 = table(forest_train$Category,forest_pred_train)
forest_accuracy_train = sum(diag(forest.conf.mat1))/sum(forest.conf.mat1)
forest_precision_train = forest.conf.mat1[2,2]/sum(forest.conf.mat1[,2])
forest_recall_train = forest.conf.mat1[2,2]/sum(forest.conf.mat1[2,])
forest_Specificity_train <- forest.conf.mat1[1,1]/sum(forest.conf.mat1[1,])

# Generating the confusion metric on test data
forest.conf.mat2 = table(forest_test$Category,forest_pred_test)

# Calculating the accuracy of the model on test data
forest_accuracy_test = sum(diag(forest.conf.mat2))/sum(forest.conf.mat2)
forest_precision_test = forest.conf.mat2[2,2]/sum(forest.conf.mat2[,2])
forest_recall_test = forest.conf.mat2[2,2]/sum(forest.conf.mat2[2,])
forest_specificity_test = forest.conf.mat2[1,1]/sum(forest.conf.mat2[1,])

#####################ADABOOST##################
ada_train <- train
ada_test <- test
ada_train$IdMedication <- NULL
ada_test$IdMedication <- NULL

library(ada)
ada_x = subset(ada_train, select = -Category) 
ada_y = as.factor(ada_train$Category) 

ada_a = subset(ada_test, select = -Category)
ada_b = as.factor(ada_test$Category)

ada_model = ada(ada_x, ada_y, iter=20, loss="logistic") 

#Predict the values using model on test data sets.
ada_pred = predict(ada_model, ada_a) 
ada_pred
#Calculate precision, recall and accuracy 
ada_result <- table(ada_pred, ada_b)
ada_result 

ada_accuracy_test = sum(diag(ada_result))/sum(ada_result)
ada_precision_test = ada_result[2,2]/sum(ada_result[,2])
ada_recall_test = ada_result[2,2]/sum(ada_result[2,])
ada_specificity_test = ada_result[1,1]/sum(ada_result[1,])





