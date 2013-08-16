#!/usr/bin/env Rscript
# clean-up session parameters
rm(list=ls())

library(kernlab)
library(e1071)

# load data
data(iris)

# set seed
#set.seed(12345)

# split data into train/test datasets
index <- 1:nrow(iris)
# get testindex using sample
testindex <- sample(index, trunc(length(index)/3))
testset <- iris[testindex,]
trainset <- iris[-testindex,]

# run svm algorithm (e1071 library) for given vector of data and kernel
#model <- svm(Species~., data=trainset, kernel="radial", gamma=0.01)
model <- svm(Species~., data=iris, kernel="radial", gamma=0.01)
print(model)
# the last column of this dataset is what we'll predict, so we'll exclude it
#prediction <- predict(model, testset[,-ncol(testset)])
prediction <- predict(model, iris[,-ncol(iris)])
# the last column is what we'll check against for
#tab <- table(pred = prediction, true = testset[,ncol(testset)])
tab <- table(pred = prediction, true = iris[,ncol(iris)])
print(tab)
cls <- classAgreement(tab)
msg <- sprintf("Correctly classified: %f, kappa %f", cls$diag, cls$kappa)
print(msg)
