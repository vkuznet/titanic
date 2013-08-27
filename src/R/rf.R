#!/usr/bin/env Rscript
# clean-up session parameters
#rm(list=ls())

do.rf <- function(tdf, testdata, drops=NULL, fname="rf", mtry=NULL,
                    formula=NULL, testindex=NULL, printModel=FALSE) {

    # drop requested attributes
    if(!is.null(drops))
        tdf <- drop(tdf, drops)

    train.df <- tdf
    test.df <- train.df
    # use 70/30 splitting
    if (is.null(testindex)) {
        print(sprintf("Run RandomForest, use full training set"))
        survived <- train.df$Survived
        pids <- train.df$PassengerId
    } else {
        testset <- train.df[testindex,]
        trainset <- train.df[-testindex,]
        train.df <- trainset
        test.df <- testset
        survived <- testset$Survived
        pids <- testset$PassengerId
        print(sprintf("Run RandomForest, train %d, test %d", nrow(trainset), nrow(testset)))
    }

    # exclude id/PassengerId columns to work with ML
    test.df.copy <- test.df
    train.df <- drop(train.df, c("id", "PassengerId"))
    test.df <- drop(test.df, c("id", "PassengerId", "Survived"))

    # run RandomForest, make sure that the variable used for classification is a
    # factor. For prediction use the same dataset but exclude classification var.
    if (is.null(formula)) formula <- as.formula("Survived~.")
    print(formula)
    if (is.null(mtry)) {
        rf.model <- randomForest(formula, data=train.df, importance=T, proximity=F)
    } else {
        rf.model <- randomForest(formula, data=train.df, importance=T, proximity=F, mtry=mtry)
    }
    rf.pred <- predict(rf.model, test.df)
    if(printModel==TRUE) print(rf.model)

    # print confugtion matrix
    if(!is.null(testindex)) {
        pfile <- sprintf("%s_prediction_test.csv", fname)
#        mdf <- misclassified(test.df.copy, rf.pred)
    } else {
        pfile <- sprintf("%s_prediction.csv", fname)
    }
    write.prediction(rf.model, testdata, pfile)
    conf.matrix(survived, rf.pred, printTable=F)

    # make RF plots
#    fig.name <- paste0("rf1", ext)
#    start.plot(fig.name)
#    par(mfrow=c(1,1))
#    plot(rf.model)
#    dev.off()
#    fig.name <- paste0("rf2", ext)
#    start.plot(fig.name)
#    par(mfrow=c(1,2))
#    varImpPlot(rf.model)
#    dev.off()

    # print important variables used in RF
    imp <- importance(rf.model)
    if(printModel==TRUE) print(imp)

    #fig.name <- paste0("partial_rf", ext)
    #start.plot(fig.name)
    #impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
    #op <- par(mfrow=c(2, 3))
    #for (i in seq_along(impvar)) {
    #    partialPlot(rf, train.df[-ncol(train.df)], impvar[i], xlab=impvar[i],
    #                main=paste("Partial Dependence on", impvar[i]))
    #}
    #par(op)
    #dev.off()

    return(int.pred(rf.pred))
}
