# load libraries
library(vcd)
library(foreign)
library(corrplot)
library(ggplot2)
library(randomForest)
library(e1071)
library(kernlab)
library(nnet)
library(MASS)
library(XML)
library(digest)
library(Hmisc)

# set seed
set.seed(12345)

source("src/R/plot_helpers.R")
source("src/R/data_helpers.R")

### Helper function
download <- function(url, dir="./data") {
    dir.create(dir, showWarnings = FALSE)
    dest.file <- paste(dir, digest(url, algo="md5"), sep="/")
    if  (!file.exists(dest.file)) {
        cat("download url", url, "\n")
        download.file(url, dest=dest.file)
    }
    return(dest.file)
}

# helper function to order data frame for given set of attributes
df.order <- function(x, x.order) {
    d <- x[with(x, order(x.order)), ]
    return(d)
}

# helper function to drop columns from given dataset
drop <- function(xdf, drops) {
    return(xdf[,!names(xdf) %in% drops])
}
# helper function to drop columns from given dataset
keep <- function(xdf, keeps) {
    return(xdf[,names(xdf) %in% keeps])
}

# helper function to get sample index for given dataframe
# then someone can divided training/test set into the following:
#    testset <- df[testindex,]
#    trainset <- df[-testindex,]
sample.index <- function(x) {
    # this is an example on how to split data into train/test datasets
    index <- 1:nrow(x)
    testindex <- sample(index, trunc(length(index)/3))
    return(testindex)
}

# helper function to dump on stdout rows with NA
na.rows <- function(x) {
    print(unique(is.na(x)))
}

# helper function to calculate error of predicion
pred.error <- function(obs, pred) {
    error <- sqrt((sum((obs-pred)^2))/length(obs))
    print(sprintf("Prediction error: %f", error))
}

# convert predicition vector into vector of ints
int.pred <- function(p) {
    # convert pred into integers
    pred <- sapply(p, function(x) {as.integer(as.character(x))})
    return(pred)
}

# Helper fuction to build and print confution matrix for given observeraion and
# prediction variables
conf.matrix <- function(obs, pred, printTable=TRUE) {
    obs <- int.pred(obs)
    pred <- int.pred(pred)
    # build confusion matrix
    tab <- table(observed = obs, predicted = pred)
    cls <- classAgreement(tab)
    if(printTable==TRUE) print(tab)
    msg <- sprintf("Correctly classified: %f, kappa %f", cls$diag, cls$kappa)
    print(msg)
    pred.error(obs, pred)
}

# merge prediction
merge.pred <- function(fits.df, printTable=TRUE) {
    pids <- fits.df$pid
    obs <- fits.df$Survived
    pred <- rowSums(fits.df[3:ncol(fits.df)])/2
    pred <- sapply(pred, function(x) {if(x==0.5) return(1) else return(as.integer(x))})
    conf.matrix(obs, pred, printTable)
}

# helper function to print misclassified rows
misclassified <- function(xdf, pred) {
    ndf <- data.frame()
    for(i in 1:nrow(xdf)) {
        row <- xdf[i,]
        if(row$Survived != pred[i]) {
            ndf <- rbind(ndf, row)
        }
    }
    print(ndf)
    print(sprintf("Misclassified: %d", nrow(ndf)))
}

roc <- function(fit, df, fname) {
    pr <- predict(fit, newdata=df, type="prob")[,2]
    pred <- prediction(pr, df$Survived)

    # get performance data for various parameters, e.g. sensitivity, etc.
    perf.ss <- performance(pred, "sens", "spec")
    perf.roc <- performance(pred, "tpr", "fpr")
    perf.lift <- performance(pred, "lift", "rpp")
    perf.pp <- performance(pred, "prec", "rec")
    perf.auc <- performance(pred, "auc")

    # make final plot of performance data
    fig.name <- paste0(fname, ext)
    start.plot(fig.name)
    par(mfrow=c(2,2))
    plot(perf.roc)
    plot(perf.ss)
    plot(perf.pp)
    plot(perf.lift)
    dev.off()
}

# helper function to write out model prediction into CSV file (kaggle format)
write.prediction <- function(model, testdata, fname="prediction.csv") {
    out <- predict(model, testdata)
    o <- sapply(out, function(x) {if(is.na(x)) return(0) else as.integer(x)})
    if (max(o) == 2) {
        o <- sapply(out, function(x) {if(is.na(x)) return(0) else as.integer(x)-1})
    }
    d <- data.frame(PassengerId=testdata$PassengerId)
    d <- cbind(d, Survived=o)
    write.csv(d, fname)
    cmd <- sprintf("cat %s | sed -e \"s,\\\",,g\" | awk '{z=split($0,a,\",\"); print \"\"a[2]\",\"a[3]\"\"}' > %s.tmp; mv %s.tmp %s", fname, fname, fname, fname)
    system(cmd)
}

# helper function to divide original dataframe into two ones based on cut
# usage. Should be run as following:
# d1=cut1(train.data)
# d2=cut2(train.data, d1)
cut1 <- function(x) {
#    d <- subset(x, (x$Age<18 & x$Pclass<3) | (x$Sex=="female" & x$Pclass<3) |
#                    x$Embarked=="" | as.integer(x$Cabin)!=1)
    d <- subset(x, (x$Age<18 & x$Pclass<3) | x$Sex=="female" |
                    x$Embarked=="" | as.integer(x$Cabin)!=1 )
    return(d)
}
cut2 <- function(x, c1) {
    ids <- c1$PassengerId
    ndf <- data.frame()
    for(i in 1:nrow(x)) {
        if (!(x[i,]$PassengerId %in% ids)) {
            ndf <- rbind(ndf, x[i,])
        }
    }
    return(ndf)
}

# RandomForest function which takes: x - train dataset, y - test dataset
run.rf <- function(tdf) {
    cat("\n### Run RandomForest\n")
    # run RandomForest, make sure that the variable used for classification is a
    # factor. For prediction use the same dataset but exclude classification var.
    rf.model <- randomForest(as.factor(Survived)~., data=tdf, importance=T, proximity=T)
    print(rf.model)

    # print important variables used in RF
    imp <- importance(rf.model)
    print(imp)
    return(rf.model)
}

run.ksvm <- function(x) {
    cat("\n### Run KSVM\n")
    # PolyKernels
    poly <- polydot(degree=1, scale=1, offset=0)
    # RBF kernels
    rbf <- rbfdot(sigma=1)

    # kernel choice
    k <- rbf

    # type of classification
    type <- sprintf("C-svc")
    cross <- 10
    cost <- 1

    # run svm algorithm
    ksvm.model <- ksvm(as.factor(Survived)~., data=x, type=type, cross=cross, kernel=k, C=cost, prob.model=T)
    print(ksvm.model)
    return(ksvm.model)
}
run.svm <- function(x) {
    cat("\n### Run SVM\n")
    # kernels
#    k <- sprintf('polynomial')
    k <- sprintf('radial')
    degree <- 3
    gamma <- 1
    type <- sprintf('C-classification')
    cross <- 10
    cross <- 0

    # run svm algorithm (e1071 library) for given vector of data and kernel
    svm.model <- svm(as.factor(Survived)~., data=x,
                 type=type, cross=cross, kernel=k, gamma=gamma, degree=degree)
    print(svm.model)
    return(svm.model)
}

# helper function to print out misclassified rows for given dataset and
# prediction
print.misses <- function(x, p) {
    cat("### misses\n")
    mdf <- data.frame()
    for(i in 1:nrow(x)) {
        if(x[i,]$Survived != int.pred(p[i])) {
            mdf <- rbind(mdf, x[i,])
        }
    }
    print(mdf)
}

cut.proc <- function(x, real.test=NULL, fname="rf") {
    d1 <- cut1(x)
    d2 <- cut2(x, d1)
    par(mfrow=c(2,1))
    hist(d1$Survived)
    hist(d2$Survived)
    d1 <- preprocess(d1)
    d2 <- preprocess(d2)
    d1 <- drop(d1, c("id", "PassengerId"))
    d2 <- drop(d2, c("id", "PassengerId"))
    test.d1 <- drop(d1, c("id", "PassengerId", "Survived"))
    test.d2 <- drop(d2, c("id", "PassengerId", "Survived"))
    f1 <- run.rf(d1)
    f2 <- run.rf(d2)
    pred1 <- predict(f1, test.d1)
    print.misses(d1, pred1)
    pred2 <- predict(f2, test.d2)
    print.misses(d2, pred2)
    cat("\n### Fit dataset1 ###\n")
    conf.matrix(d1$Survived, pred1)
#    imp1 <- importance(f1)
#    print(imp1)
    cat("\n### Fit dataset2 ###\n")
    print(f2)
    conf.matrix(d2$Survived, pred2)
#    imp2 <- importance(f2)
#    print(imp2)
    if (!is.null(real.test)) {
        test1 <- cut1(real.test)
        test2 <- cut2(real.test, test1)
        test1 <- preprocess(test1, survived=F)
        test2 <- preprocess(test2, survived=F)
        test1 <- drop(test1, c("id", "Survived"))
        test2 <- drop(test2, c("id", "Survived"))
        fname1 <- paste0(fname, "1.csv")
        fname2 <- paste0(fname, "2.csv")
        write.prediction(f1, test1, fname1)
        write.prediction(f2, test2, fname2)
        cmd <- sprintf("cat %s %s | sort -n -u > %s_double.csv", fname1, fname2, fname)
        system(cmd)
    }
}

# helper function to break down attribute into binary form for a given dataset
break.down <- function(x, attr, drop=T) {
    values <- unique(x[,c(attr)])
    for(i in 1:nrow(x)) {
        val <- x[i,c(attr)]
        for(j in values) {
            cname <- sprintf("%s.%i", attr, j)
            if (val == j) {
                x[i,c(cname)] <- 1
            } else {
                x[i,c(cname)] <- 0
            }
        }
    }
    if (drop==T) {
        d <- drop(x, c(attr))
    } else {
        d <- x
    }
    return(d)
}
