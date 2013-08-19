#!/usr/bin/env Rscript
# clean-up session parameters
rm(list=ls())

# load libraries, helper functions, set seed.
source("src/R/helper.R")

# load data
my.path <- paste0(getwd(), "/")
file.name <- "model.csv"
df <- read.csv(paste0(my.path, file.name), header=T)

# exclude id columne to work with ML
train.df <- df[2:ncol(df)]
# during training we use the same dataset, but exclude classification var (last one)
test.df <- train.df[2:ncol(train.df)-1]

# this is an example on how to split data into train/test datasets
#index <- 1:nrow(df)
#testindex <- sample(index, trunc(length(index)/3))
#testset <- df[testindex,]
#trainset <- df[-testindex,]

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
model <- ksvm(Survived~., data=train.df, type=type, cross=cross, kernel=k, C=cost)
print(model)

# the last column of this dataset is what we'll predict, so we'll exclude it
ksvm.pred <- predict(model, test.df)

# print confugtion matrix
conf.matrix(train.df$Survived, ksvm.pred)
