# Data Helpers functions

# helper function to assign ticket id
assign.tid <- function(t, tickets) {
    if(is.na(t)) return(0)
    return (which(tickets==as.character(t)))
}

# helper function to assign cabin id
assign.cid <- function(t, cabins) {
    if(is.na(t)) return(0)
    return (which(cabins==as.character(t)))
}

# helper function to adjust Parch
adjust.Parch <- function(r) {
#    if(r$Parch>3) {
#        r$Parch <- 3
#    }
    return(r$Parch)
}

# helper function to adjust SibSp
adjust.SibSp <- function(r) {
#    if(r$SibSp>3) {
#        r$SibSp <- 3
#    }
    return(r$SibSp)
}

# helper function to break down fare into N bins
assign.fbin <- function(r, step=10, nbins=10) {
    if (is.na(r$Fare)) return(0)
    for(n in seq(1, nbins)) {
        min.bound <- (n-1)*step
        max.bound <- n*step
        if (r$Fare>min.bound & r$Fare<=max.bound) return(n)
    }
    return(nbins)
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
    } else if(grepl("Col\\. ", r$Name)) {
        title <- 3
    } else {
        title <- 0
    }
    return(title)
}

# helper function to assign Age
# tweak NAs of Age attribute as following:
# parch is number of parents/children abroad
# sibsp is number of siblings/spouses abroad
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
    return(age)
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

