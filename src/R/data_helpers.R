# Data Helpers functions

# helper function to convert given list of attributes into binary form
to.binary <- function(x, attr) {
    if(!is.null(attrs)) return(x)
    for(a in attrs) {
        if(length(as.integer(which(names(x)==a)))>0)
            x <- break.down(x, a)
    }
    return(x)
}

# helper function to assign embarked id
assign.eid <- function(r) {
    if(is.na(r$Embarked)) return(1)
    x <- as.character(r$Embarked)
    if(x=="C") return(1)
    else if(x=="S") return(2)
    else if(x=="Q") return(3)
    else return(1) # missing value
}

# helper function to assign ticket id
assign.tid <- function(r, tickets) {
    t <- r$Ticket
#    if(is.na(t) | as.character(t) == "") return(0)
#    return(1)
    return (which(tickets==as.character(t)))
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
            if(r$Age<thr) {
                w <- 9
            } else {
                w <- 10
            }
        } else {
            if(r$Age<thr) {
                w <- 8
            } else {
                w <- 4
            }
        }
    } else if (r$Pclass==2) {
        if(r$Sex=="female") {
            if(r$Age<thr) {
                w <- 10
            } else {
                w <- 9
            }
        } else {
            if(r$Age<thr) {
                w <- 6
            } else {
                w <- 2
            }
        }
    } else {
        if(r$Sex=="female") {
            if(r$Age<thr) {
                w <- 6
            } else {
                w <- 5
            }
        } else {
            if(r$Age<thr) {
                w <- 3
            } else {
                w <- 2
            }
        }
    }
    return(w)
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
assign.title <- function(r) {
    title <- 0
    if(grepl("Miss\\. ", r$Name)) {
        title <- 1
    } else if(grepl("Mrs\\. ", r$Name)) {
        title <- 2
    } else if(grepl("Lady\\. ", r$Name)) {
        title <- 2
    } else if(grepl("Mr\\. ", r$Name)) {
        title <- 3
    } else if(grepl("Dr\\. ", r$Name)) {
        title <- 3
    } else if(grepl("Master\\. ", r$Name)) {
        title <- 4
    } else if(grepl("Capt\\. ", r$Name)) {
        title <- 5
    } else {
        title <- 0
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
assign.age <- function(r, adult.age, kid.age) {
    age <- r$Age
    if(is.na(r$Age) & grepl("Mrs\\. ", r$Name)) {
        age <- adult.age
    }
    else if(is.na(r$Age) & grepl("Miss\\. ", r$Name)) {
        age <- kid.age
    }
    else if(is.na(r$Age) & grepl("Master\\. ", r$Name)) {
        age <- kid.age
    }
    else if(is.na(r$Age) & grepl("Mr\\. ", r$Name)) {
        age <- adult.age
    }
    else if (is.na(r$Age) & r$Parch>2) {
        age <- adult.age
    }
    else if(is.na(r$Age) & r$SibSp>1) {
        age <- kid.age
    }
    else if(is.na(r$Age) & !r$SibSp & !r$Parch) {
        age <- adult.age
    }
    else if(is.na(r$Age)){
#        age <- adult.age
        age <- kid.age
    }
    return(round(age))
}

# return either kid or adult
assign.child <- function(r, thr) {
    kid <- 1
    adult <- 0
    if(is.na(r$Age)) {
        if (r$Parch>2) {
            return(adult)
        }
        else if(grepl("Mrs\\. ", r$Name)) {
            return(adult)
        }
        else if(grepl("Miss\\. ", r$Name)) {
            return(kid)
        }
        else if(grepl("Master\\. ", r$Name)) {
            return(kid)
        }
        else if(grepl("Mr\\. ", r$Name)) {
            return(adult)
        }
        else if(grepl("Dr\\. ", r$Name)) {
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

# helper function to re-assign family attribute based on ticket info
adjust.family <- function(x) {
    d <- x[with(x, order(-TicketId, -Family)), ]
    tid <- -1
    for(i in 1:nrow(d)) {
        row <- d[i,]
        pids <- d[d$TicketId==row$TicketId,]$PassengerId
        if(length(pids)>1) { # rows with the same tickets
            for(pid in pids) {
                d[i,]$Family <- 1
            }
        }
    }
    d <- d[with(d, order(PassengerId)),]
    return (d)
}

