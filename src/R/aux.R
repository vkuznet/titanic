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
        gsub("\\]", "", gsub("\\]", "", x))
        return(x)
    })
    x$Crew <- sapply(x$Class,
    function(x) {
        x <- as.character(x)
        if(x=="Engineering Crew") return(1)
        else if(x=="Deck Crew") return(2)
        else if(x=="Victualling Crew") return(3)
        else if(x=="Restaurant Staff") return(4)
        else if(x=="Victualling CrewPostal Clerk") return(5)
        else return(0)
    })
    x$Servant.1 <- sapply(x$Class,
    function(x) {
        x <- as.character(x)
        if(x=="1st Class PassengerServant") return(1)
        else if(x=="1st Class PassengerH&W Guarantee Group") return(1)
        else return(0)
    })
    x$Servant.2 <- sapply(x$Class,
    function(x) {
        x <- as.character(x)
        if(x=="2nd Class PassengerServant") return(1)
        else if(x=="2nd Class PassengerH&W Guarantee Group") return(1)
        else return(0)
    })
    x$Class <- sapply(x$Class,
    function(x) {
        x <- as.character(x)
        if(x=="1st Class Passenger") return(1)
        else if(x=="2nd Class Passenger") return(2)
        else if(x=="3rd Class Passenger") return(3)
        else if(x=="Engineering Crew") return(4)
        else if(x=="Deck Crew") return(5)
        else if(x=="Victualling Crew") return(6)
        else if(x=="1st Class PassengerServant") return(7)
        else if(x=="Restaurant Staff") return(8)
        else if(x=="2nd Class PassengerServant") return(9)
        else if(x=="1st Class PassengerH&W Guarantee Group") return(10)
        else if(x=="2nd Class PassengerMusician") return(11)
        else if(x=="2nd Class PassengerH&W Guarantee Group") return(12)
        else if(x=="Victualling CrewPostal Clerk") return(13)
        else return(14)
    })
#    x$Pclass <- sapply(x$Class,
#    function(x) {
#        if(grepl("1st", x)==T) return(1)
#        else if(grepl("2nd", x)==T) return(2)
#        else if(grepl("3rd", x)==T) return(3)
#        else if(grepl("Crew", x)==T) return(4)
#        else if(grepl("Staff", x)==T) return(5)
#        else return(5)
#    })
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
    jid <- as.integer(jid)
    return (as.integer(jid)) 
}

# helper function to adjust titanica data to the form suitable for my analysis
aux.data <- function() {
    survivors <- read.survivors()
    victims <- read.victims()
    job.1 <- sapply(survivors$Job, function(x) {as.character(x)})
    job.2 <- sapply(victims$Job, function(x) {as.character(x)})
    f.jobs <- unique(c(job.1, job.2))
    ndf <- data.frame()
    for(i in 1:nrow(survivors)) {
        row <- survivors[i,]
        row$jid <- assign.jid(row, f.jobs)
        ndf <- rbind(ndf, row)
    }
    for(i in 1:nrow(victims)) {
        row <- victims[i,]
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
            # override Age, Emabarked and Pclass
            if(is.na(xrow$Age)) {
#                print(sprintf("Assign age: %s, %s, %s", xrow$Name, arow$Name, arow$Age))
                xrow$Age <- arow$Age
            }
        } else if(length(nid2)==1) {
            arow <- adf[nid2,]
            # add job and family ids
            xrow$jid <- arow$jid
            # override Age, Emabarked and Pclass
            if(is.na(xrow$Age)) {
#                print(sprintf("Assign age: %s, %s, %s", xrow$Name, arow$Name, arow$Age))
                xrow$Age <- arow$Age
            }
        } else if(length(nid3)==1) {
            arow <- adf[nid3,]
            # add job and family ids
            xrow$jid <- arow$jid
            # override Age, Emabarked and Pclass
            if(is.na(xrow$Age)) {
#                print(sprintf("Assign age: %s, %s, %s", xrow$Name, arow$Name, arow$Age))
                xrow$Age <- arow$Age
            }
        } else {
            print(sprintf("Non-identified: %s", as.character(xrow$Name)))
            xrow$jid <- 0
        }
        xrow$Embarked <- arow$Embarked
        xrow$Class <- arow$Class
#        xrow$Crew <- arow$Crew
        xrow$Servant.1 <- arow$Servant.1
        xrow$Servant.2 <- arow$Servant.2
        xrow$Boat <- arow$Boat
        xrow$fname <- arow$fname
        ndf <- rbind(ndf, xrow)
    }
    return(ndf)
}
