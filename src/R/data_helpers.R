# Data Helpers functions

# helper function for title assignment
isMaster <- function(x) {
    x <- tolower(x)
    return(grepl("master\\. ", x) | grepl(" master ", x))
}
isMiss <- function(x) {
    x <- tolower(x)
    return(grepl("miss\\. ", x) | grepl(" miss ", x))
}
isMr <- function(x) {
    x <- tolower(x)
    return(grepl("mr\\. ", x) | grepl(" mr ", x) | grepl(" fr ", x) | grepl(" dr ", x) | grepl(" sir ", x))
}
isMrs <- function(x) {
    x <- tolower(x)
    return(grepl("mrs\\. ", x) | grepl(" mrs ", x) | grepl(" lady ", x))
}

# helper function to assign SW (survival weight) attribute based on 
# number of people with the same ticket and their inverse Age
assign.SW <- function(x, tickets) {
    t <- x$T
    if(x$Age>0) {
        w <- 1/x$Age
    } else {
        w <- 1/mean(x$Age)
    }
    sur <- x$Survived
    ntickets <- length(which(tickets==as.integer(t)))
    delta <- 0.3
    if(x$Pclass==1) f<-1
    else if(x$Pclass==1) f<-2
    else f<-3
    if(ntickets>0) {
        return(w*ntickets/f)
#        if(is.null(sur)) return(0.5)
#        else if(as.character(sur)=="0") return(runif(1,0,0.5+delta))
#        else if(as.character(sur)=="1") return(runif(1,0.5-delta,1))
#        else return(0.5)
    }
#    return(0.5)
    return(w/f)
}

# helper function to convert given list of attributes into binary form
to.binary <- function(x, attrs) {
    if(is.null(attrs)) return(x)
    for(a in attrs) {
        if(length(as.integer(which(names(x)==a)))>0)
            x <- break.down(x, a)
    }
    return(x)
}

# helper function to assing ticket
assign.ticket <- function(r) {
    tname <- as.character(r)
    if(tname=="LINE") return(0)
    if(tname=="") return(0)
    if(tname=="NA") return(0)
    tlist <- unlist(strsplit(tname, " "))
    ticket <- gsub("[A-Za-z]", "", tlist[length(tlist)])
    ticket <- gsub(" ", "", ticket)
    return(as.integer(ticket))
}

# helper function to assign short name, return lowercase name
assign.sname <- function(r) {
    fname <- tolower(as.character(r))
    s <- unlist(strsplit(gsub("[,\\.]", "", fname), " "))
    sname <- paste(paste(s[1], s[2], sep=" "), s[3], sep=" ")
    master <- grepl(" master ", sname)
    sname <- gsub(" master ", " mr ", sname)
    sname <- gsub(" lady ", " mrs ", sname)
    sname <- gsub(" sir ", " mr ", sname)
    sname <- gsub("ö", "o", sname)
    if(length(s)>3) sname <- paste(sname, s[4], sep=" ")
    if(grepl(" jr", fname) | master) sname <- paste0(sname, " jr")
    return(tolower(sname))
}

# helper function to assign name id
assign.fid <- function(row, fam.names) {
    person <- sapply(row$Name, function(x) {tolower(unlist(strsplit(as.character(x), ","))[1])})
    fid <- which(fam.names==person)
    if(length(fid) > 1 | length(fid)==0){
        print(sprintf("FOUND multiple fids or fid=0"))
        print(row)
    }
#    if (person=="ware") print(sprintf("Name: %s, fid: %d", person, fid))
    return (as.integer(fid)) 
}

# helper function to assign embarked id
assign.eid <- function(r) {
    if(is.na(r$Embarked)) return(1)
    x <- as.character(r$Embarked)
    if(x=="C") return(1)
    else if(x=="S") return(2)
    else if(x=="Q") return(3)
    else return(4) # missing value
}

# helper function to assign ticket id
assign.tid <- function(r, tickets) {
    t <- r$Ticket
#    if(is.na(t) | as.character(t) == "") return(0)
#    return(1)
    return (which(tickets==as.character(t)))
}

# helper function to assign survival cabin id
assign.scid <- function(r, sur.cabins) {
    cabin <- as.character(r$Cabin)
    res <- which(sur.cabins==cabin)
    if(length(res)>0) return(1)
    else return(0)
}
# helper function to assign survival job id
assign.jid <- function(r, sur.jids) {
    print(r)
    if(is.na(r$jid)) return(0)
    res <- which(sur.jids==r$jid)
    if(length(res)>0) return(1)
    else return(0)
}
# helper function to assign survival family name
assign.sfname <- function(r, sur.fnames) {
    fname <- unlist(strsplit(as.character(r$Name), ","))[1]
    fname <- tolower(fname)
    fname <- capitalize(fname)
    res <- which(sur.fnames==fname)
    if(length(res)>0) return(1)
    else return(0)
}
# helper function to assign survival ticket id
assign.sticket <- function(r, sur.tickets) {
    t <- r$T
    res <- which(sur.tickets==t)
    if(length(res)>0) return(1)
    else return(0)
}



# helper function to assign cabin id
assign.cid <- function(r, cabins) {
    t <- r$Cabin
    if(is.na(t) | as.character(t) == "") return(0)
    return(1)
#    return (which(cabins==as.character(t)))
}

# helper function to adjust Parch
adjust.Parch <- function(r) {
    if(r$Parch>3) {
        r$Parch <- 3
    }
    return(r$Parch)
}

# helper function to adjust SibSp
adjust.SibSp <- function(r) {
    if(r$SibSp>3) {
        r$SibSp <- 3
    }
    return(r$SibSp)
}

# helper function to break down given attribute into N bins
assign.bin <- function(r, attr, bins) {
    if (is.na(r[attr])) return(0)
    for(idx in 2:length(bins)) {
        min.bound <- bins[idx-1]
        max.bound <- bins[idx]
        if (r[attr]>min.bound & r[attr]<=max.bound) return(idx-1)
    }
    return(length(bins))
}

# helper function to add weigths based on Sex, Age, Pclass
# The logic is based on mosaic plot
assign.weigth <- function(r, thr) {
    w <- 1 # default weight
    if (r$Pclass==1) {
        if(r$Sex=="female") {
            if(r$Age<=thr) {
                w <- 1
            } else {
                w <- 95
            }
        } else {
            if(r$Age<=thr) {
                w <- 99
            } else {
                w <- 40
            }
        }
    } else if (r$Pclass==2) {
        if(r$Sex=="female") {
            if(r$Age<=thr) {
                w <- 99
            } else {
                w <- 90
            }
        } else {
            if(r$Age<=thr) {
                w <- 95
            } else {
                w <- 10
            }
        }
    } else {
        if(r$Sex=="female") {
            if(r$Age<=thr) {
                w <- 40
            } else {
                w <- 50
            }
        } else {
            if(r$Age<=thr) {
                w <- 40
            } else {
                w <- 15
            }
        }
    }
    w <- (w/100)
    return(w)
}
assign.wf <- function(x) {
    if(r$Family==1) {
        w <- 1
    } else {
        w <- 0.2
    }
    return(w)
}
assign.wc <- function(x) {
    if(r$cid==1) {
        wc <- 1
    } else {
        wc <- 0.7
    }
    return(wc)
}
assign.we <- function(x) {
    f.L <- 10
    f.M <- 30
    if(r$Embarked==1) { # Embarked=C
        if(r$cid==1) {
            if(r$Fare<=f.L) we <- 50
            else if(r$Fare>f.L&r$Fare<f.M) we <- 60
            else we <- 80
        } else {
            if(r$Fare<=f.L) we <- 25
            else if(r$Fare>f.L&r$Fare<f.M) we <- 50
            else we <-60
        }
    } else if(r$Embarked==2) { # Embarked=S
        if(r$cid==1) {
            if(r$Fare<=f.L) we <- 30
            else if(r$Fare>f.L&r$Fare<f.M) we <- 80
            else we <- 70
        } else {
            if(r$Fare<=f.L) we <- 15
            else if(r$Fare>f.L&r$Fare<f.M) we <- 35
            else we <- 30
        }
    } else { # Embarked=Q
        if(r$cid==1) {
            if(r$Fare<=f.L) we <- 10
            else if(r$Fare>f.L&r$Fare<f.M) we <- 90
            else we <- 50
        } else {
            if(r$Fare<=f.L) we<- 40
            else if(r$Fare>f.L&r$Fare<f.M) we <- 30
            else we <- 50
        }
    }
    we <- (we/100)
    return(we)
}

# helper function to assign family name
assign.fname <- function(r) {
    x <- as.character(r$Name)
    fname <- unlist(strsplit(x, ","))[1]
    fname <- tolower(fname)
#    return(capitalize(fname))
    return(fname)
}

# assign family attribute based on
# Parch: number of parents/children
# SibSp: number of siblings/spouses
assign.family <- function(r) {
    if((r$Parch+r$SibSp)>0) return(1)
    return(0)
}

# helper function to assign Title
# make new Title category: 1-Miss, 2-Mrs, 3-Mr, 4-Dr, 0-otherwise
# 1-Miss or Mrs, 2-Mr, 3-Master
assign.title <- function(r) {
    title <- 0
    if(isMiss(r$Name)) {
        title <- 1
    } else if(isMrs(r$Name)) {
        title <- 1
    } else if(isMr(r$Name)) {
        title <- 2
    } else if(isMaster(r$Name)) {
        title <- 3
    } else {
        title <- 2 # was zero
    }
    return(title)
}

# helper function to assign Age
# tweak NAs of Age attribute as following:
# parch is number of parents/children abroad
# sibsp is number of siblings/spouses abroad
# if parch == 0 it means this is adult
# if sibsp == 0 it means this is adult
# if parch > 2 it means this is adult
# if sibsp > 1 it means this is a child
# if sibsp=parch=0, it means it was adult
# if Name has Mrs, it meas married woman
# if Name has Miss, it meas un-married woman, so we'll assign a kid
# if Name has Mr., we'll assign an adult
assign.age <- function(xdf, r, adult.age, kid.age) {
    age <- r$Age
    if(is.na(age)) {
        if(isMrs(r$Name)) {
            age <- adult.age
        }
        else if(isMiss(r$Name)) {
            age <- adult.age
        }
        else if(isMaster(r$Name)) {
            age <- kid.age 
        }
        else if(isMr(r$Name)) {
            age <- adult.age
        }
        else if(r$Parch>=0 &r$Parch<=2) {
            if(n.tickets(xdf, r$PassengerId)==0)
            age <- adult.age
            else
            age <- kid.age
        }
        else if(r$SibSp>=0 & r$SibSp<=2) {
            if(n.tickets(xdf, r$PassengerId)==0)
            age <- adult.age
            else
            age <- kid.age
        }
        else if (r$Parch>2) {
            age <- adult.age
        }
        else if(r$SibSp>1) {
            age <- kid.age
        }
        else {
            age <- adult.age
        }
        print(sprintf("Adjust age for %s, %s", r$Name, age))
    }
    age <- round(age)
    if(age==0) age <- 1
    return(age)
}

# helper function to find number of tickets for given PassengerId
n.tickets <- function(x, pid) {
    # find tid for given pid
    tid <- x[x$PassengerId==pid,]$tid
    # find rows with given tid
    tdf <- x[x$tid==tid,]
    return(nrow(tdf))
}

# return either kid or adult
assign.child <- function(r, thr) {
    kid <- 1
    adult <- 0
    if(is.na(r$Age)) {
        if (r$Parch>2) {
            return(adult)
        }
        else if(isMrs(r$Name)) {
            return(adult)
        }
        else if(isMiss(r$Name)) {
            return(adult)
        }
        else if(isMaster(r$Name)) {
            return(kid)
        }
        else if(isMr(r$Name)) {
            return(adult)
        }
        else if (!r$Parch) {
            return(adult)
        }
        else if (r$Parch==1|r$Parch==2) {
            return(kid)
        }
        else if(r$SibSp>1) {
            return(kid)
        }
        else {
            return(adult)
        }
    } else {
        if (r$Age>thr) {
            return(adult)
        }
        else {
            return(kid)
        }
    }
    return(adult)
}

# helper function to assign Fare
assign.fare <- function(r) {
    if(is.na(r$Fare)) return(5)
    return(round(r$Fare))
}

# helper function to assign gender
assign.gender <- function(r) {
    if(r$Sex=="male") return(1)
    return(0)
}

# helper function to assign Cabin category
cabin.category <- function(r) {
    x <- r$Cabin
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
adjust.cabin <- function(x) {
    d <- x[with(x, order(-tid, -ccat)), ]
    tid <- -1
    for(i in 1:nrow(d)) {
        if (tid == d[i,]$tid) { # tickets are the same
            d[i,]$ccat <- d[i-1,]$ccat
        } else {
            tid <- d[i,]$tid
        }
    }
    d <- d[with(d, order(PassengerId)),]
    return (d)
}

# helper function to re-assign family attribute based on ticket info
adjust.family.v1 <- function(x) {
    d <- x[with(x, order(-tid, -Family)), ]
    tid <- -1
    for(i in 1:nrow(d)) {
        row <- d[i,]
        pids <- d[d$tid==row$tid,]$PassengerId
        if(length(pids)>1) { # rows with the same tickets
            for(pid in pids) {
                d[i,]$Family <- 1
            }
        }
    }
    d <- d[with(d, order(PassengerId)),]
    return (d)
}

adjust.family <- function(x) {
    d <- x[with(x, order(-fid, -Family)), ]
    fid <- -1
    for(i in 1:nrow(d)) {
        row <- d[i,]
        pids <- d[d$fid==row$fid,]$PassengerId
        if(length(pids)>1) { # rows with the same tickets
            for(pid in pids) {
                d[i,]$Family <- 1
            }
        }
    }
    d <- d[with(d, order(PassengerId)),]
    return (d)
}

write.model <- function(xdf, drops=NULL, keeps=NULL, fname="model") {

    # drop requested attributes
    if(!is.null(drops))
        xdf <- drop(xdf, drops)
    else if(!is.null(keeps))
        xdf <- keep(xdf, keeps)
    else 
        xdf <- xdf
    print(names(xdf))

    # write data out for SVM
    f.csv <- sprintf("%s.csv", fname)
    print(sprintf("Write %s", f.csv))
    write.csv(xdf, file=f.csv)
    cmd=sprintf("cat %s | sed 's/\"\"/\"id\"/g' > t.csv; mv -f t.csv %s", f.csv, f.csv)
    system(cmd)

    # write data in arff format for Weka
    f.arff <- sprintf("%s.arff", fname)
    print(sprintf("Write %s", f.arff))
    write.arff(xdf, file=f.arff)
    cmd=sprintf("cat %s | sed \"s/Survived numeric/Survived {0,1}/g\" > t.arff; mv -f t.arff %s", f.arff, f.arff)
    system(cmd)
    cmd=sprintf("cat %s | sed -e \"s/Survived string/Survived {0,1}/g\" -e \"s/'//g\" > t.arff; mv -f t.arff %s", f.arff, f.arff)
    system(cmd)
}
