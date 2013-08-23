#!/usr/bin/env Rscript
# clean-up session parameters
#rm(list=ls())

do.svm <- function(tdf, testdata, fname="svm", split=FALSE, printModel=FALSE) {
    # exclude id columne to work with ML
    train.df <- drop(tdf, c("id", "PassengerId"))
    # during training we use the same dataset, but exclude classification var
    test.df <- drop(train.df, c("Survived"))
    survived <- train.df$Survived

    # use 70/30 splitting
    if (split==FALSE) {
        print(sprintf("Run SVM, use full training set"))
    } else {
        index <- 1:nrow(train.df)
        testindex <- sample(index, trunc(length(index)/3))
        testset <- train.df[testindex,]
        trainset <- train.df[-testindex,]
        train.df <- trainset
        test.df <- drop(testset, c("Survived"))
        survived <- testset$Survived
        print(sprintf("Run SVM, train %d, test %d", nrow(trainset), nrow(testset)))
    }

    # kernels
    k <- sprintf('polynomial')
    degree <- 3
    gamma <- 1
    type <- sprintf('C-classification')
    cross <- 10

    # run svm algorithm (e1071 library) for given vector of data and kernel
    svm.model <- svm(Survived~., data=train.df,
                 type=type, cross=cross, kernel=k, gamma=gamma, degree=degree)
    if(printModel==TRUE) print(svm.model)

    # the last column of this dataset is what we'll predict, so we'll exclude it
    svm.pred <- predict(svm.model, test.df)

    # write out prediction
    pfile <- sprintf("%s_prediction.csv", fname)
    write.prediction(svm.model, testdata, pfile)

    # print confugtion matrix
    conf.matrix(survived, svm.pred)
}
