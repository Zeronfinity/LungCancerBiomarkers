rm(list=ls())

library("e1071")

setwd("E:/Academic/4-1/Thesis/Serum and Plasma Metabolomic/plasma/")

load("train_test_features_class.RData")
load("dem_t_test.RData")
load("dem_k_wallis.RData")
load("dem_wilcoxon.RData")

DEMetabols = intersect(ttMeta, kwMeta)
DEMetabols = intersect(DEMetabols, wilMeta)

featuresTrain = featuresTrain[,DEMetabols]
featuresTest = featuresTest[,DEMetabols]
classTrain <- as.factor(classTrain)

svm_model <- svm(classTrain ~ ., data = featuresTrain, kernel="radial")
summary(svm_model)

system.time(predicted <- predict(svm_model, featuresTest))

mat <- table(predicted, classTest)

accuracy1 <- sum(diag(mat)) / sum(mat)

system.time(svm_tune <- tune(svm, train.x=featuresTrain, train.y=as.factor(classTrain), 
                 kernel="radial", ranges=list(cost=1, gamma=seq(0.0123, 0.01235, 0.000001) )))

print(svm_tune)

svm_model_after_tune <- svm(classTrain ~ ., data = featuresTrain, kernel="radial", cost=svm_tune$best.parameters$cost, gamma=svm_tune$best.parameters$gamma)
#summary(svm_model_after_tune)

system.time(predicted <- predict(svm_model_after_tune,featuresTest))

mat = table(predicted, classTest)

accuracy2 <- sum(diag(mat)) / sum(mat)

print(accuracy1)
print(svm_tune$best.parameters$cost)
print(svm_tune$best.parameters$gamma)
print(accuracy2)