#!/usr/bin/env Rscript
# clean-up session parameters
#rm(list=ls())

source("src/R/helper.R")

# helper function to read titanica data for given url
read.titanica <- function(url) {
    dir <- "./data"
    dir.create(dir, showWarnings = FALSE)
    dest.file <- paste(dir, digest(url, algo="md5"), sep="/")
    csv.file <- paste0(dest.file, ".csv")
    if(file.exists(csv.file)) {
        print(sprintf("Read %s", csv.file))
        d <- read.csv(csv.file, header=T)
    } else {
        data <- readHTMLTable(download(url))
        d <- data$myTable
        names(d) <- c("id", "Name", "Age", "Class", "Ticket", "Embarked", "Job", "Boat")
        write.csv(d, csv.file)
    }
    return(d)
}

# helper function to process titanica data
process.titanica <- function(x) {
    x$Embarked <- sapply(x$Embarked,
    function(x) {
        if(x=="Cherbourg") return("C")
        else if(x=="Southampton") return("S")
        else if(x=="Queenstown") return("Q")
        else return("B") # Belfast
    })
    x$Boat <- sapply(x$Boat,
    function(x) {
        gsub("\\[", "", gsub("\\[", "", x))
    })
    x$Class <- sapply(x$Class,
    function(x) {
        if(grepl("1st", x)==T) return(1)
        else if(grepl("2nd", x)==T) return(2)
        else if(grepl("3rd", x)==T) return(3)
        else if(grepl("Crew", x)==T) return(4)
        else if(grepl("Staff", x)==T) return(5)
        else return(6)
    })
    x$T <- sapply(x$Ticket,
    function(x) {
        if(is.na(x) | as.character(x)=="" | as.character(x)=="NA") return(0)
        ticket <- unlist(strsplit(as.character(x), "£"))[1]
        ticket <- gsub("[A-Za-z]", "", ticket)
        ticket <- gsub(" ", "", ticket)
        return(as.integer(ticket))
    })
    x$sname <- sapply(x$Name, assign.sname)
    x$fname <- sapply(x$Name,
    function(x) {
        sname <- unlist(strsplit(as.character(x), ","))[1]
        sname <- tolower(sname)
        return(capitalize(sname))
    })
    x$Age <- sapply(x$Age,
    function(x) {
        if(grepl("m$", x)==T) {
            age <- as.integer(strsplit(as.character(x), "m")[1])/12.
        } else {
            age <- as.character(x)
        }
        return(as.double(age))
    })
    return(x)
}

# helper function to read survivors from titanica web site
read.survivors <- function() {
    url <- "http://www.encyclopedia-titanica.org/titanic-survivors/"
    return(process.titanica(read.titanica(url)))
}

# helper function to read victims from titanica web site
read.victims <- function() {
    url <- "http://www.encyclopedia-titanica.org/titanic-victims/"
    return(process.titanica(read.titanica(url)))
}

# helper function to assign job id
assign.jid <- function(xdf, jobs) {
    job <- as.character(xdf$Job)
    jid <- which(jobs==job)
    return (as.integer(jid)) 
}

# helper function to adjust titanica data to the form suitable for my analysis
aux.data <- function() {
    survivors <- read.survivors()
    victims <- read.victims()
#    f.names <- unique(c(survivors$fname, victims$fname))
    job.1 <- sapply(survivors$Job, function(x) {as.character(x)})
    job.2 <- sapply(victims$Job, function(x) {as.character(x)})
    f.jobs <- unique(c(job.1, job.2))
    ndf <- data.frame()
    for(i in 1:nrow(survivors)) {
        row <- survivors[i,]
#        row$fid <- assign.fid(row, f.names)
        row$jid <- assign.jid(row, f.jobs)
        ndf <- rbind(ndf, row)
    }
    for(i in 1:nrow(victims)) {
        row <- victims[i,]
#        row$fid <- assign.fid(row, f.names)
        row$jid <- assign.jid(row, f.jobs)
        ndf <- rbind(ndf, row)
    }
    return (ndf)
}

# helper function to merge aux data with provided dataset, we fill out missing
# Age attribute and append fid, jid values
merge.with.aux <- function(adf, xdf) {
    ndf <- data.frame()
    for(i in 1:nrow(xdf)) {
        xrow <- xdf[i,]
        nid1 <- which(as.character(xrow$sname)==as.character(adf$sname) & xrow$T==adf$T)
        nid2 <- which(as.character(xrow$sname)==as.character(adf$sname))
        nid3 <- which(xrow$T==adf$T)
        if(length(nid1)==1) {
            arow <- adf[nid1,]
            xrow$jid <- arow$jid
#            xrow$fid <- arow$fid
            # override Age, Emabarked and Pclass
#            if(is.na(xrow$Age))
                xrow$Age <- arow$Age
        } else if(length(nid2)==1) {
            arow <- adf[nid2,]
            # add job and family ids
            xrow$jid <- arow$jid
#            xrow$fid <- arow$fid
            # override Age, Emabarked and Pclass
#            if(is.na(xrow$Age))
                xrow$Age <- arow$Age
        } else if(length(nid3)==1) {
            arow <- adf[nid3,]
            # add job and family ids
            xrow$jid <- arow$jid
#            xrow$fid <- arow$fid
            # override Age, Emabarked and Pclass
#            if(is.na(xrow$Age))
                xrow$Age <- arow$Age
        } else {
            print(sprintf("Non-identified: %s", as.character(xrow$Name)))
            xrow$jid <- 0
#            xrow$fid <- 0
        }
        xrow$Embarked <- arow$Embarked
        xrow$Pclass <- arow$Class
        ndf <- rbind(ndf, xrow)
    }
    return(ndf)
}
