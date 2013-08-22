#!/usr/bin/env Rscript
# clean-up session parameters
#rm(list=ls())

do.ksvm <- function(tdf, testdata, fname="ksvm") {
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
    rbf <- rbfdot(sigma=1)
    cost <- 1
    # so far sigma=3 and C=3 gave 93.5%
    #rbf <- rbfdot(sigma=3)
    #cost <- 3
    # caret suggested based on 25/75 split: sigma=0.0879 and C=2
    #rbf <- rbfdot(sigma=0.0879)
    #cost <- 2

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
