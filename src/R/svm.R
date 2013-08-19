#!/usr/bin/env Rscript
# clean-up session parameters
#rm(list=ls())

# load data
my.path <- paste0(getwd(), "/")
file.name <- "model.csv"
df <- read.csv(paste0(my.path, file.name), header=T)

# SVM library
library(e1071)

# set seed
set.seed(1)

# exclude id columne to work with ML
train.df <- df[2:ncol(df)]
# during training we use the same dataset, but exclude classification var (last one)
test.df <- train.df[2:ncol(train.df)-1]

# this is an example on how to split data into train/test datasets
#index <- 1:nrow(df)
#testindex <- sample(index, trunc(length(index)/3))
#testset <- df[testindex,]
#trainset <- df[-testindex,]

# kernels
k <- sprintf('polynomial')
degree <- 3
gamma <- 1
type <- sprintf('C-classification')
cross <- 10

# run svm algorithm (e1071 library) for given vector of data and kernel
model <- svm(as.factor(Survived)~., data=train.df,
             type=type, cross=cross, kernel=k, gamma=gamma, degree=degree)
print(model)

# the last column of this dataset is what we'll predict, so we'll exclude it
svm.pred <- predict(model, test.df)

# build confusion matrix
tab <- table(observed = train.df$Survived, predicted = svm.pred)
cls <- classAgreement(tab)
msg <- sprintf("Correctly classified: %f, kappa %f", cls$diag, cls$kappa)
print(msg)

