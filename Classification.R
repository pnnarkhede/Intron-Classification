##############################################################################
# PACKAGES: To facilitate the analysis, load the following packages.         #
# (Please ensure you have installed the latest version of each package.)     #
##############################################################################
library(randomForest)
library(party)
library(pROC)
library(kernlab)
library(caret)
library(e1071)
library(class)
library(xgboost)
library(ROCR)

##############################################################################
# DATA IMPORT: Prepare the dataset for analysis.                             #
##############################################################################

mydata<-as.data.frame(read.csv("mydata",header = TRUE))

set.seed(2000)

##############################################################################
#                                                                            #
#    CLASSIFICATION WORKFLOWS: Deployment of multiple supervised learning    #
#    models (each described below) and performance evaluation based on       #
#    receiver operating characteristic (ROC) data.                           #
#                                                                            #
##############################################################################

control = trainControl(method="cv", number=10)

# K-NEAREST NEIGHBORS: Non-parametric classification prediction #

knn.model = train(Group ~ ., data = traindata, method="knn",
                  preProcess="scale", trControl=control)
knn.model
summary(knn.model)

knn.pred<-predict(knn.model,newdata=data.frame(testdata[, !names(testdata) %in% c("Group")]))
knn.result<-confusionMatrix(test$Group,knn.pred)
knn.result

knn.probs<-predict(knn.model,newdata=data.frame(testdata[, !names(testdata) %in% c("Group")])
                   ,type="prob")

knn.ROC <- roc(testdata$Group,knn.probs[,2])
knn.ROC$auc


# SUPPORT VECTOR MACHINE: Non-linear classification using radial basis function #

svm.model= train(Group ~ .,
                 data = traindata,
                 method = "svmRadial",
                 metric = "ROC",
                 trControl = ccontrol)
svm.model

summary(svm.model)

svm.pred<-predict(svm.model,newdata=data.frame(testdata[, !names(testdata) %in% c("Group")]))
svm.result<-confusionMatrix(test$Group,svm.pred)
svm.result

svm.probs<-predict(svm.model,newdata=data.frame(testdata[, !names(testdata) %in% c("Group")])
                   ,type="prob")

svm.ROC <- roc(testdata$Group,svm.probs[,2])
svm.ROC$auc

# RANDOM FOREST: Implementation of RF algorithm using traditional decision trees #

rf.model = train(Group ~ ., 
                 data = traindata, 
                 method="rf",
                 preProcess="scale", 
                 trControl=control)
rf.model#
summary(rf.model)

rf.pred<-predict(rf.model,newdata=data.frame(testdata[, !names(testdata) %in% c("Group")]))
rf.result<-confusionMatrix(testdata$Group,rf.pred)
rf.result
rf.probs<-predict(rf.model,newdata=data.frame(testdata[, !names(testdata) %in% c("Group")])
                  ,type="prob")
rf.ROC <- roc(testdata$Group,rf.probs[,2])
rf.ROC$auc#


# CFOREST: Implementation of RF algorithm using conditional inference trees #

cforest.model = train(Group ~ ., data = traindata, method="cforest",
                      preProcess="scale", trControl=control)


cforest.model
summary(cforest.model)

cforest.pred<-predict(cforest.model,newdata=data.frame(testdata[, !names(testdata) %in% c("Group")]))
cforest.result<-confusionMatrix(testdata$Group,cforest.pred)
cforest.result
cforest.probs<-predict(cforest.model,newdata=data.frame(testdata[, !names(testdata) %in% c("Group")])
                       ,type="prob")

cforest.ROC <- roc(testdata$Group,cforest.probs[,2])

cforest.ROC$auc



# XGBOOST: Gradient-boosting machine learning framework #

xgb.model <-xgboost(data = traindata$data, 
                     label = traindata$label, 
                      max.depth = 4, 
                       eta = 0.5)

xgb.model
summary(xgb.model)

xgb.pred<-predict(xgb.model,newdata=testdata$data)

result<-table(testdata$label,xgb.pred)


xgb.probs<-predict(xgb.model,newdata=testdata$data
                   ,type="prob")

xgb.probs<-sapply(xgb.probs,'[',2)


xgb.ROC <- roc(testdata$label.probs)

xgb.ROC$auc
