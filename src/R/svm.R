#!/usr/bin/env Rscript
# clean-up session parameters
#rm(list=ls())

do.svm <- function(tdf, testdata, keeps, fname="svm", testindex=NULL, printModel=FALSE) {
    # keep requested attributes
    if(!is.null(keeps))
        tdf <- keep(tdf, keeps)

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

    # kernels
    k <- sprintf('polynomial')
    degree <- 3
    gamma <- 1
    type <- sprintf('C-classification')
    cross <- 10

    # run svm algorithm (e1071 library) for given vector of data and kernel
    if (is.null(formula)) formula <- as.formula("Survived~.")
    print(formula)
    svm.model <- svm(formula, data=train.df,
                 type=type, cross=cross, kernel=k, gamma=gamma, degree=degree)
    if(printModel==TRUE) print(svm.model)

    # the last column of this dataset is what we'll predict, so we'll exclude it
    svm.pred <- predict(svm.model, test.df)

    # print confugtion matrix
    if(!is.null(testindex)) {
        conf.matrix(survived, svm.pred, printTable=T)
        mdf <- misclassified(test.df.copy, svm.pred)
    } else {
        # write out prediction
        pfile <- sprintf("%s_prediction.csv", fname)
        write.prediction(svm.model, testdata, pfile)

        conf.matrix(survived, svm.pred, printTable=F)
    }

    return(int.pred(svm.pred))
}
