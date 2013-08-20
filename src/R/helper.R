# load libraries
library(vcd)
library(foreign)
library(corrplot)
library(ggplot2)
library(randomForest)
library(e1071)
library(kernlab)

# set seed
set.seed(12345)

# use PDF for high-quality plots, while png for rmd
#ext <- ".pdf"
ext <- ".png"

# helper function to initialize plotting function depending on file extention
start.plot <- function(f) {
    if (grepl("png$", f))
        png(f, bg="transparent")
    else if (grepl("pdf$", f))
        pdf(f)
    else if (grepl("jpg$", f))
        jpeg(f)
}

# helper functino to reset plot layout
graph.reset <- function() {
    par(mfrow=c(1,1))
    dev.off()
}

# helper function to drop columns from given dataset
drop <- function(df, drops) {
    return(df[,!names(df) %in% drops])
}

# helper function to assign Cabin category
cabin.category <- function(x) {
    llist <- LETTERS
    cat <- 0
    for(i in 1:length(llist)) {
        pat <- sprintf("^%s" , llist[i])
        if (grepl(pat, x)) {
            return(i)
        }
    }
    return(cat)
}

# helper function to re-assign cabin based on ticket info
assign.cabin <- function(x) {
    d <- x[with(x, order(-TicketId, -CabinCat)), ]
    tid <- -1
    for(i in 1:nrow(d)) {
        if (tid == d[i,]$TicketId) { # tickets are the same
            d[i,]$CabinCat <- d[i-1,]$CabinCat
        } else {
            tid <- d[i,]$TicketId
        }
    }
    d <- d[with(d, order(PassengerId)),]
    return (d)
}

# make plots for various attributes of given dataframe
make.plots <- function(df.orig) {
    print(sprintf("Make plots"))
    # make new data frame with characters for mosaic plot
    df.ch <- data.frame(Sex=df.orig$Sex)
    df.ch$Class <- sapply(df.orig$Pclass,
    function(x) {
        if(x==1) return("1st")
        else if(x==2) return("2nd")
        else if(x==3) return("3rd")
        else return("Crew")
    })
    #df.ch$Sex <- sapply(df.orig$Sex, function(x) {if(x=="male") return("Male") else return("Female")})
    df.ch$Sec <- df.orig$Sex
    df.ch$Survived <- sapply(df.orig$Survived, function(x) {if(x==1) return("Yes") else return("No")})
    df.ch$Age <- sapply(df.orig$Age,
    function(x) {
        if(is.na(x)) return("NA")
        else if(x>age.thr) return("Adult")
        else return("Child")
    })

    # make mosaic plot of few attributes
    fig.name <- paste0("mosaic", ext)
    start.plot(fig.name)
    mosaic(~Class+Sex+Age+Survived, data=df.ch, shade=TRUE, legend=TRUE)
    dev.off()

    # make plots of SibSp, Ticket, Parch vs Pclass
    sdf <- subset(df.orig, df.orig$Survived==1)
    nsdf <- subset(df.orig, df.orig$Survived==0)

    fig.name <- paste0("class_plots1", ext)
    start.plot(fig.name)
    par(mfrow=c(2,2))
    plot(sdf$SibSp, sdf$Pclass, xlab="SibSp", ylab="Class", main="Survived")
    plot(nsdf$SibSp, nsdf$Pclass, xlab="SibSp", ylab="Class", main="Not-Survived")
    plot(sdf$Parch, sdf$Pclass, xlab="Parch", ylab="Class", main="Survived")
    plot(nsdf$Parch, nsdf$Pclass, xlab="Parch", ylab="Class", main="Not-Survived")
    dev.off()
    fig.name <- paste0("class_plots2", ext)
    start.plot(fig.name)
    par(mfrow=c(2,2))
    plot(sdf$Age, sdf$Pclass, xlab="Age", ylab="Class", main="Survived")
    plot(nsdf$Age, nsdf$Pclass, xlab="Age", ylab="Class", main="Not-Survived")
    plot(sdf$Embarked, sdf$Pclass, xlab="Embarked", ylab="Class", main="Survived")
    plot(nsdf$Embarked, nsdf$Pclass, xlab="Embarked", ylab="Class", main="Not-Survived")
    dev.off()
    fig.name <- paste0("class_plots3", ext)
    start.plot(fig.name)
    par(mfrow=c(1,2))
    plot(sdf$Sex, sdf$Pclass, xlab="Sex", ylab="Class", main="Survived")
    plot(nsdf$Sex, nsdf$Pclass, xlab="Sex", ylab="Class", main="Not-Survived")
    dev.off()
    fig.name <- paste0("class_plots4", ext)
    start.plot(fig.name)
    par(mfrow=c(1,2))
    hist(df.orig$Survived, main="Survived rate", xlab="no cuts")
    hist(subset(df.orig, (df.orig$Age<18 & df.orig$Pclass<3) | (df.orig$Sex=="female" & df.orig$Pclass<3) | df.orig$Embarked=="")$Survived, main="Survived rate", xlab="age<18&class<3|sex=F&class<3|not-embarked")
    dev.off()
}

# make correlation plot
make.cor.plot <- function (df) {
    print(sprintf("Make correlation plot"))
    # make correlation matrix
    idx <- 2
    cor.matrix <- cor(df[,idx:ncol(df)])
    colnames(cor.matrix) <- names(df)[idx:ncol(df)]
    rownames(cor.matrix) <- names(df)[idx:ncol(df)]
    correlations <- as.numeric(cor.matrix)

    # round to 2 digits correlation matrix and plot it
    fig.name <- paste0("cor", ext)
    start.plot(fig.name)
    cor <- round(cor.matrix, digits=2)
    corrplot(cor, method="shade", shade.col=NA, tl.col="black", tl.srt=45, addCoef.col="black", bg="transparent")
    dev.off()
    cat("====== Correlations ======\n")
    print(cor)
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
conf.matrix <- function(obs, pred) {
    pred <- int.pred(pred)
    # build confusion matrix
    tab <- table(observed = obs, predicted = pred)
    cls <- classAgreement(tab)
    print(tab)
    msg <- sprintf("Correctly classified: %f, kappa %f", cls$diag, cls$kappa)
    print(msg)
    pred.error(obs, pred)
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
