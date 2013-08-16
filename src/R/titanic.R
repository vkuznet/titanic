#!/usr/bin/env Rscript
# clean-up session parameters
rm(list=ls())

#library(fields)
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

# always set a seed
set.seed(12345)

preprocess <- function(df.orig) {

    # Take all numeric attributes from original df and put it into new dataframe
    df <- data.frame(id=df.orig$PassengerId,
                     SibSp=df.orig$SibSp,
                     Parch=df.orig$Parch)

    # Conver class into binary
    #df$Class <- df.orig$Pclass
    df$Class.1 <- sapply(df.orig$Pclass, function(x) {if(x==1) return(1) else return(0)})
    df$Class.2 <- sapply(df.orig$Pclass, function(x) {if(x==2) return(1) else return(0)})
    df$Class.3 <- sapply(df.orig$Pclass, function(x) {if(x==3) return(1) else return(0)})
    # convert Gender Male to 1, Female to 0
    df$Gender <- sapply(df.orig$Sex, function(x) {if(x=="male") return(1) else return(0)})

    # put Fare in bins
    df$Fare <- df.orig$Fare
#    df$Fare.1 <- sapply(df.orig$Fare, function(x) {if(x<100) return(1) else return(0)})
#    df$Fare.2 <- sapply(df.orig$Fare, function(x) {if(x>=100) return(1) else return(0)})
    #df$Fare.3 <- sapply(df.orig$Fare, function(x) {if(x>100) return(1) else return(0)})

    # convert Embarked attribute into binary form

    #df$Embarked <- sapply(df.orig$Embarked,
    #function(x) {
    #    if(x=="C") return(1)
    #    else if(x=="Q") return(2)
    #    else if(x=="S") return(3)
    #    else return(0)
    #})

    df$Embarked.C <- sapply(df.orig$Embarked, function(x) {if(x=="C") return(1) else return(0)})
    df$Embarked.Q <- sapply(df.orig$Embarked, function(x) {if(x=="Q") return(1) else return(0)})
    df$Embarked.S <- sapply(df.orig$Embarked, function(x) {if(x=="S") return(1) else return(0)})

    # convert Age into binary form of three categoris:
    # 0: Child, age < 12
    # 1: Adult
    # 2: Unknown
    #df$Age.category <- sapply(df.orig$Age, function(x) {if(is.na(x)) return(2) else if(x>12) return(1) else return(0)})

    # trick to drop columne
    #drops <- c("Age")
    #df <- df[,!names(df) %in% drops]

    # binary age categories
    age.thr <- 8
    df$Child <- sapply(df.orig$Age, function(x) {if(!is.na(x) & x<age.thr) return(1) else return(0)})
    #df$Adult <- sapply(df.orig$Age, function(x) {if(is.na(x) | x>=age.thr) return(1) else return(0)})
    df$Adult <- sapply(df.orig$Age, function(x) {if(!is.na(x) & x>=age.thr) return(1) else return(0)})
    df$Age.NA <- sapply(df.orig$Age, function(x) {if(is.na(x)) return(1) else return(0)})

    # add classifier as last column
    df$Survived <- df.orig$Survived

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

    library(vcd)
    fig.name <- paste0("mosaic", ext)
    start.plot(fig.name)
    mosaic(~Class+Sex+Age+Survived, data=df.ch, shade=TRUE, legend=TRUE)
    dev.off()

    # make correlation matrix
    idx <- 2
    cor.matrix <- cor(df[,idx:ncol(df)])
    colnames(cor.matrix) <- names(df)[idx:ncol(df)]
    rownames(cor.matrix) <- names(df)[idx:ncol(df)]
    correlations <- as.numeric(cor.matrix)
    cat("====== Correlations ======\n")
    print(cor.matrix)

    # round to 2 digits correlation matrix and plot it
    fig.name <- paste0("cor", ext)
    start.plot(fig.name)
    cor <- round(cor.matrix, digits=2)
    corrplot(cor, method="shade", shade.col=NA, tl.col="black", tl.srt=45, addCoef.col="black")
    dev.off()

    # return our data frame
    return(df)
}

df <- preprocess(data)

# drop some attribute before writing
drops <- c("id", "Age", "Class")
df <- df[,!names(df) %in% drops]
# write data out for SVM
write.csv(df, file="model.csv")
cmd="cat model.csv | sed 's/\"\"/\"id\"/g' > t.csv; mv -f t.csv model.csv"
system(cmd)

# write data in arff format for Weka
write.arff(df, file="model.arff")
cmd="cat model.arff  | sed \"s/Survived numeric/Survived {0,1}/g\" > t.arff; mv -f t.arff model.arff"
system(cmd)
