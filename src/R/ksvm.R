#!/usr/bin/env Rscript
# clean-up session parameters
#rm(list=ls())

do.ksvm <- function(tdf, testdata, fname="ksvm", sigma=1, cost=1) {
    # exclude id columne to work with ML
    train.df <- drop(tdf, c("id", "PassengerId"))
    # during training we use the same dataset, but exclude classification var
    test.df <- drop(train.df, c("Survived"))

    # this is an example on how to split data into train/test datasets
    #index <- 1:nrow(df)
    #testindex <- sample(index, trunc(length(index)/3))
    #testset <- df[testindex,]
    #trainset <- df[-testindex,]

    ###### kernels

    # PolyKernels
    poly <- polydot(degree=1, scale=1, offset=0)
    # RBF kernels
    sigma <- sigma
    rbf <- rbfdot(sigma=sigma)

    # kernel choice
    k <- rbf

    # type of classification
    type <- sprintf("C-svc")
    cross <- 10

    # run svm algorithm
    ksvm.model <- ksvm(Survived~., data=train.df,
                type=type, cross=cross, kernel=k, C=cost, prob.model=T)
    print(ksvm.model)

    # the last column of this dataset is what we'll predict, so we'll exclude it
    ksvm.pred <- predict(ksvm.model, test.df)

    # write out prediction
    pfile <- sprintf("%s_prediction.csv", fname)
    write.prediction(ksvm.model, testdata, pfile)

    # print confugtion matrix
    conf.matrix(train.df$Survived, ksvm.pred)
}
