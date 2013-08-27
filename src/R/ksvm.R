#!/usr/bin/env Rscript
# clean-up session parameters
#rm(list=ls())

do.ksvm <- function(tdf, testdata, drops=NULL, fname="ksvm", sigma=1, cost=1,
                        formula=NULL, testindex=NULL, printModel=FALSE) {
    # drop requested attributes
    if(!is.null(drops))
        tdf <- drop(tdf, drops)

    train.df <- tdf
    test.df <- train.df
    # use 70/30 splitting
    if (is.null(testindex)) {
        print(sprintf("Run KSVM, use full training set"))
        survived <- train.df$Survived
        pids <- train.df$PassengerId
    } else {
        testset <- train.df[testindex,]
        trainset <- train.df[-testindex,]
        train.df <- trainset
        test.df <- testset
        survived <- testset$Survived
        pids <- testset$PassengerId
        print(sprintf("Run KSVM, train %d, test %d", nrow(trainset), nrow(testset)))
    }

    # exclude id/PassengerId columns to work with ML
    test.df.copy <- test.df
    train.df <- drop(train.df, c("id", "PassengerId"))
    test.df <- drop(test.df, c("id", "PassengerId", "Survived"))

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
    if (is.null(formula)) formula <- as.formula("Survived~.")
    print(formula)
    ksvm.model <- ksvm(formula, data=train.df,
                type=type, cross=cross, kernel=k, C=cost, prob.model=T)
    if(printModel==TRUE) print(ksvm.model)

    # the last column of this dataset is what we'll predict, so we'll exclude it
    ksvm.pred <- predict(ksvm.model, test.df)

    # print confugtion matrix
    if(!is.null(testindex)) {
        conf.matrix(survived, ksvm.pred, printTable=T)
#        mdf <- misclassified(test.df.copy, ksvm.pred)
    } else {
        # write out prediction
        pfile <- sprintf("%s_prediction.csv", fname)
        write.prediction(ksvm.model, testdata, pfile)

        conf.matrix(survived, ksvm.pred, printTable=F)
    }

    return(int.pred(ksvm.pred))
}
