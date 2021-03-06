# Plot Helper functions

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
    df.ch$Sex <- df.orig$Sex
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
    df.ch$Embarked <- sapply(df.orig$Embarked,
    function(x) {
        if(x==1) return("C")
        else if(x==2) return("S")
        else if(x==3) return("Q")
        else return("B")
    })
    df.ch$Cabin <- sapply(df.orig$cid, function(x){if(x==1) return("Yes") else return("No")})
    df.ch$Fare <- sapply(df.orig$Fare,
    function(x) {
        if(x<=10) return("L")
        else if(x>10&x<=30)
        return("M")
        else return("H")
    })
    fig.name <- paste0("mosaic2", ext)
    start.plot(fig.name)
    mosaic(~Embarked+Cabin+Fare+Survived, data=df.ch, shade=TRUE, legend=TRUE)
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

    # survival plots by class
    brks <- seq(0,80,5)
    fig.name <- paste0("survival_age", ext)
    start.plot(fig.name)
    par(mfrow=c(1,1))
    p0=hist(subset(train.data, train.data$Survived==0)$Age, breaks=brks, plot=F)
    p1=hist(subset(train.data, train.data$Survived==1)$Age, breaks=brks, plot=F)
    plot(p0, col=rgb(0,0,1,1/4), xlim=c(0,80), main="Survived", xlab="blue/red non/survived")
    plot(p1, col=rgb(1,0,0,1/4), xlim=c(0,80), add=T, main="Survived", xlab="blue/red non/survived")
    dev.off()

    # survival plot for fare
#    brks <- seq(0,max(max(sdf$Fare), max(ndf$Fare))+10,10)
#    fig.name <- paste0("survival_fare", ext)
#    start.plot(fig.name)
#    par(mfrow=c(1,1))
#    p0=hist(sdf$Fare, plot=F, breaks=breaks)
#    p1=hist(ndf$Fare, plot=F, breaks=breaks)
#    plot(p1, col=rgb(1,0,1,1/4), xlim=c(0,500), main="Survived", xlab="blue/red non/survived")
#    plot(p0, col=rgb(0,0,1,1/4), xlim=c(0,500), add=T, main="Survived", xlab="blue/red non/survived")
#    dev.off()
}

# make correlation plot
make.cor.plot <- function (x, fname="cor", drops=NULL) {
    # drop requested attributes
    if(!is.null(drops))
        x <- drop(x, drops)

    df <- drop(x, c("Survived", "PassengerId"))
    # make correlation matrix
    idx <- 2
    cor.matrix <- cor(df[,idx:ncol(df)])
    colnames(cor.matrix) <- names(df)[idx:ncol(df)]
    rownames(cor.matrix) <- names(df)[idx:ncol(df)]
    correlations <- as.numeric(cor.matrix)

    # round to 2 digits correlation matrix and plot it
    fig.name <- paste0(fname, ext)
    print(sprintf("Make correlation plot %s", fig.name))
    start.plot(fig.name)
    cor <- round(cor.matrix, digits=2)
    corrplot(cor, method="shade", shade.col=NA, tl.col="black", tl.srt=45, addCoef.col="black", bg="transparent")
    dev.off()
#    cat("====== Correlations ======\n")
#    print(cor)
}

