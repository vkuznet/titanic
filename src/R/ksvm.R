#!/usr/bin/env Rscript
# clean-up session parameters
rm(list=ls())

# load data
my.path <- paste0(getwd(), "/")
file.name <- "model.csv"
df <- read.csv(paste0(my.path, file.name), header=T)
#print(head(df))

# SVM library
library(kernlab)

# set seed
set.seed(1)

# split data into train/test datasets
index <- 1:nrow(df)
# get testindex using sample
testindex <- sample(index, trunc(length(index)/3))
testset <- df[testindex,]
trainset <- df[-testindex,]

###### kernels

# PolyKernels
poly <- polydot(degree=1, scale=1, offset=0)
# RBF kernels
rbf <- rbfdot(sigma=1)

# kernel choice
k <- rbf

# type of classification
type <- sprintf("C-svc")
cross <- 10
cost <- 1

# run svm algorithm
model <- ksvm(Survived~., data=trainset, type=type, cross=cross, kernel=k, C=cost)
print(model)
# the last column of this dataset is what we'll predict, so we'll exclude it
prediction <- predict(model, testset[,-ncol(testset)])
# the last column is what we'll check against for
tab <- table(pred = prediction, true = testset[,ncol(testset)])
cls <- classAgreement(tab)
msg <- sprintf("Correctly classified: %f, kappa %f", cls$diag, cls$kappa)
print(msg)
