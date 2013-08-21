#!/usr/bin/env Rscript
# clean-up session parameters
#rm(list=ls())

do.svm <- function(tdf, testdata, fname="svm") {
    # exclude id columne to work with ML
    train.df <- drop(tdf, c("id", "PassengerId"))
    # during training we use the same dataset, but exclude classification var
    test.df <- drop(tdf, c("id", "PassengerId", "Survived"))

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
    svm.model <- svm(Survived~., data=train.df,
                 type=type, cross=cross, kernel=k, gamma=gamma, degree=degree)
    print(svm.model)

    # the last column of this dataset is what we'll predict, so we'll exclude it
    svm.pred <- predict(svm.model, test.df)

    # write out prediction
    pfile <- sprintf("%s_prediction.csv", fname)
    write.prediction(svm.model, testdata, pfile)

    # print confugtion matrix
    conf.matrix(train.df$Survived, svm.pred)
}
