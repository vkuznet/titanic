#!/usr/bin/env Rscript
# clean-up session parameters
#rm(list=ls())

source("src/R/helper.R")
library(caret)

set.seed(1)

run.carret <- function(d) {
x <- d
x$Survived <- sapply(x$Survived, function(y) {as.factor(y)})
print(str(x))
inTrain <- createDataPartition(x$Survived, p = .75, list = FALSE)
trainDescr <- x[ inTrain, -ncol(x)]
testDescr  <- x[-inTrain, -ncol(x)]
trainClass <- x$Survived[ inTrain]
testClass  <- x$Survived[-inTrain]
method <- c("center", "scale")
procValues <- preProcess(trainDescr, method = method)
trainScaled <- predict(procValues, trainDescr)
testScaled  <- predict(procValues, testDescr)
rbfSVM <- train(x = trainDescr, y = trainClass,
                method = "svmRadial", preProc = method,
                ## Length of default tuning parameter grid
                tuneLength = 8,
                ## Bootstrap resampling with custon performance metrics:
                ## sensitivity, specificity and ROC curve AUC
                trControl = trainControl(method = "repeatedcv", repeats = 5),
                metric = "Kappa",
                ## Pass arguments to ksvm
                fit = FALSE)
print(rbfSVM, printCall = FALSE)
print(class(rbfSVM))
print(class(rbfSVM$finalModel))
par(mfrow=c(2,1))
plot(rbfSVM, xTrans = function(x) log2(x))
densityplot(rbfSVM, metric = "Kappa", pch = "|")

svmPred <- predict(rbfSVM, testDescr)
cm <- confusionMatrix(svmPred, testClass)
print(cm)

}
