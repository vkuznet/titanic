#!/usr/bin/env Rscript
# clean-up session parameters
#rm(list=ls())

do.rf <- function(tdf, testdata, fname="rf", mtry=NULL, split=FALSE, printModel=FALSE) {
    # exclude id columne to work with ML
    train.df <- drop(tdf, c("id", "PassengerId"))
    # during training we use the same dataset, but exclude classification var
    test.df <- drop(train.df, c("Survived"))
    survived <- train.df$Survived

    # use 70/30 splitting
    if (split==FALSE) {
        print(sprintf("Run RandomForest, use full training set"))
    } else {
        index <- 1:nrow(train.df)
        testindex <- sample(index, trunc(length(index)/3))
        testset <- train.df[testindex,]
        trainset <- train.df[-testindex,]
        train.df <- trainset
        test.df <- drop(testset, c("Survived"))
        survived <- testset$Survived
        print(sprintf("Run RandomForest, train %d, test %d", nrow(trainset), nrow(testset)))
    }

    # run RandomForest, make sure that the variable used for classification is a
    # factor. For prediction use the same dataset but exclude classification var.
    if (is.null(mtry)) {
        rf.model <- randomForest(Survived~., data=train.df, importance=T, proximity=F)
    } else {
        rf.model <- randomForest(Survived~., data=train.df, importance=T, proximity=F, mtry=mtry)
    }
    rf.pred <- predict(rf.model, test.df)
    if(printModel==TRUE) print(rf.model)

    # write out prediction
    pfile <- sprintf("%s_prediction.csv", fname)
    write.prediction(rf.model, testdata, pfile)

    # print confugtion matrix
    conf.matrix(survived, rf.pred)

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
}
