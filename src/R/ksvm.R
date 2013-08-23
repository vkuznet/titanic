#!/usr/bin/env Rscript
# clean-up session parameters
#rm(list=ls())

do.ksvm <- function(tdf, testdata, fname="ksvm", sigma=1, cost=1, split=FALSE, printModel=FALSE) {
    # exclude id columne to work with ML
    train.df <- drop(tdf, c("id", "PassengerId"))
    # during training we use the same dataset, but exclude classification var
    test.df <- drop(train.df, c("Survived"))
    survived <- train.df$Survived

    # use 70/30 splitting
    if (split==FALSE) {
        print(sprintf("Run KSVM, use full training set"))
    } else {
        index <- 1:nrow(train.df)
        testindex <- sample(index, trunc(length(index)/3))
        testset <- train.df[testindex,]
        trainset <- train.df[-testindex,]
        train.df <- trainset
        test.df <- drop(testset, c("Survived"))
        survived <- testset$Survived
        print(sprintf("Run KSVM, train %d, test %d", nrow(trainset), nrow(testset)))
    }

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
    if(printModel==TRUE) print(ksvm.model)

    # the last column of this dataset is what we'll predict, so we'll exclude it
    ksvm.pred <- predict(ksvm.model, test.df)

    # write out prediction
    pfile <- sprintf("%s_prediction.csv", fname)
    write.prediction(ksvm.model, testdata, pfile)

    # print confugtion matrix
    conf.matrix(survived, ksvm.pred)
}
