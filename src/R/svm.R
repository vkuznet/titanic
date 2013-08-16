#!/usr/bin/env Rscript
# clean-up session parameters
rm(list=ls())

# load data
my.path <- paste0(getwd(), "/")
file.name <- "model.csv"
df <- read.csv(paste0(my.path, file.name), header=T)
#print(head(df))

# SVM library
library(e1071)

# set seed
set.seed(1)

# split data into train/test datasets
index <- 1:nrow(df)
# get testindex using sample
testindex <- sample(index, trunc(length(index)/3))
testset <- df[testindex,]
trainset <- df[-testindex,]

# kernels
k <- sprintf('polynomial')
degree <- 3
gamma <- 1
type <- sprintf('C-classification')
cross <- 10

# run svm algorithm (e1071 library) for given vector of data and kernel
model <- svm(Survived~., data=trainset, type=type, cross=cross, kernel=k, gamma=gamma, degree=degree)
# run svm from kernlab
#type <- sprintf("C-svs")
#model <- ksvm(Survived~., data=trainset, seed=1, type=type, scaled=F)
print(model)
# the last column of this dataset is what we'll predict, so we'll exclude it
prediction <- predict(model, testset[,-ncol(testset)])
# the last column is what we'll check against for
tab <- table(pred = prediction, true = testset[,ncol(testset)])
cls <- classAgreement(tab)
msg <- sprintf("Correctly classified: %f, kappa %f", cls$diag, cls$kappa)
print(msg)

