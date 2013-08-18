#!/usr/bin/env Rscript
# clean-up session parameters
rm(list=ls())

library(vcd)
library(foreign)
library(corrplot)
library(ggplot2)

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

# load data
my.path <- paste0(getwd(), "/data/")
file.name <- "train.csv"
data <- read.csv(paste0(my.path, file.name), header=T)
file.name <- "test.csv"
test.data <- read.csv(paste0(my.path, file.name), header=T)

# always set a seed
set.seed(12345)

# set Age threshold to divide Adults and Children
age.thr <- 10

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

adjust.data <- function(x, debug=F) {
    # tweak NAs of Age attribute as following:
    # parch is number of parents/children abroad
    # sibsp is number of sibglings/spouses abroad
    # if parch > 2 it means this is adult
    # if sibsp > 1 it means this is a child
    # if sibsp=parch=0 and pclass=1, it means it was adult
    # if Name has Mrs, it meas married woman
    # if Name has Misss, it meas un-married woman, so we'll assign it kids age
    # if Name has Mr., we'll assign it as adult man
    ndf <- data.frame()
    adult.age <- mean(x$Age, na.rm=T)
    kid.age <- age.thr-1
    for(i in 1:nrow(x)) {
        row <- x[i,]
        if (is.na(row$Age) & row$Parch>2) {
            row$Age <- adult.age
        }
        else if(is.na(row$Age) & row$SibSp>1) {
            row$Age <- kid.age
        }
        else if(is.na(row$Age) & !row$SibSp & !row$Parch) {
            row$Age <- adult.age
        }
        else if(is.na(row$Age) & grepl("Mrs", row$Name)) {
            row$Age <- adult.age
        }
        else if(is.na(row$Age) & grepl("Miss", row$Name)) {
            row$Age <- adult.age
        }
        else if(is.na(row$Age) & grepl("Mr.", row$Name)) {
            row$Age <- adult.age
        }
        else if(is.na(row$Age)){
            row$Age <- adult.age
        }
        if (debug == T) {
            print(row)
        }
        ndf <- rbind(ndf, row)
    }
    return(ndf)
}

preprocess <- function(df.orig, survived=T) {

    # Take all numeric attributes from original df and put it into new dataframe
    df <- data.frame(id=df.orig$PassengerId,
                     SibSp=df.orig$SibSp,
                     Parch=df.orig$Parch)

    # convert Age into binary form of three categoris:
    # 0: Child, age < 12
    # 1: Adult
    # 2: Unknown
    #df$Age.category <- sapply(df.orig$Age, function(x) {if(is.na(x)) return(2) else if(x>12) return(1) else return(0)})

    df$Age <- df.orig$Age
    # binary age categories
#    df$Child <- sapply(df.orig$Age, function(x) {if(!is.na(x) & x<age.thr) return(1) else return(0)})
    #df$Adult <- sapply(df.orig$Age, function(x) {if(is.na(x) | x>=age.thr) return(1) else return(0)})
#    df$Adult <- sapply(df.orig$Age, function(x) {if(!is.na(x) & x>=age.thr) return(1) else return(0)})
#    df$Age.NA <- sapply(df.orig$Age, function(x) {if(is.na(x)) return(1) else return(0)})

    # Conver class into binary
    #df$Class <- df.orig$Pclass
    df$Class.1 <- sapply(df.orig$Pclass, function(x) {if(x==1) return(1) else return(0)})
    df$Class.2 <- sapply(df.orig$Pclass, function(x) {if(x==2) return(1) else return(0)})
    df$Class.3 <- sapply(df.orig$Pclass, function(x) {if(x==3) return(1) else return(0)})

    # convert Gender Male to 1, Female to 0
    df$Gender <- sapply(df.orig$Sex, function(x) {if(x=="male") return(1) else return(0)})

    # put Fare in bins
#    df$Fare <- df.orig$Fare
    df$Fare <- scale(df.orig$Fare)
#    df$Fare.1 <- sapply(df.orig$Fare, function(x) {if(x<100) return(1) else return(0)})
#    df$Fare.2 <- sapply(df.orig$Fare, function(x) {if(x>=100) return(1) else return(0)})
    #df$Fare.3 <- sapply(df.orig$Fare, function(x) {if(x>100) return(1) else return(0)})

    # convert Embarked attribute into binary form

#    df$Embarked <- sapply(df.orig$Embarked,
#    function(x) {
#        if(x=="C") return(1)
#        else if(x=="Q") return(2)
#        else if(x=="S") return(3)
#        else return(0)
#    })

    df$Embarked.C <- sapply(df.orig$Embarked, function(x) {if(x=="C") return(1) else return(0)})
    df$Embarked.Q <- sapply(df.orig$Embarked, function(x) {if(x=="Q") return(1) else return(0)})
    df$Embarked.S <- sapply(df.orig$Embarked, function(x) {if(x=="S") return(1) else return(0)})
    df$Embarked.NA <- sapply(df.orig$Embarked, function(x) {if(x=="") return(1) else return(0)})

    # add classifier as last column
    if (survived==T) {
        df$Survived <- df.orig$Survived
    } else {
        df$Survived <- rep("?", nrow(df.orig))
    }

    # return our data frame
    return(df)
}

# Perform data adjustments, make some plots and write out our data for ML algorithms

print(sprintf("Fix Age attribute of train data"))
print(sprintf("Prior correction: # of missing Age: %d", nrow(subset(data, is.na(data$Age)))))
data <- adjust.data(data)
print(sprintf("After correction: # of missing Age: %d", nrow(subset(data, is.na(data$Age)))))
# make some plots for our data
make.plots(data)

# Fix Age attribute of test data
print(sprintf("Fix Age attribute"))
print(sprintf("Prior correction: # of missing Age: %d", nrow(subset(test.data, is.na(test.data$Age)))))
test.data <- adjust.data(test.data)
print(sprintf("After correction: # of missing Age: %d", nrow(subset(test.data, is.na(test.data$Age)))))

# prepare data for ML algorithms
df <- preprocess(data)
# prepare test.data for ML, we do not write survived attribute
test.df <- preprocess(test.data, survived=F)

make.cor.plot(df)

# drop some attribute before writing
#drops <- c("Age", "Class")
drops <- c("Class")
df <- df[,!names(df) %in% drops]
test.df <- test.df[,!names(test.df) %in% drops]

# write data out for SVM
write.csv(df, file="model.csv")
cmd="cat model.csv | sed 's/\"\"/\"id\"/g' > t.csv; mv -f t.csv model.csv"
system(cmd)

# write data in arff format for Weka
write.arff(df, file="model.arff")
cmd="cat model.arff  | sed \"s/Survived numeric/Survived {0,1}/g\" > t.arff; mv -f t.arff model.arff"
system(cmd)
# write test data in arff format
write.arff(test.df, file="test.arff")
cmd="cat test.arff  | sed -e \"s/Survived string/Survived {0,1}/g\" -e \"s/'//g\" > t.arff; mv -f t.arff test.arff"
system(cmd)
