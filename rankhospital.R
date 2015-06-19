
# state - takes two letter state abbreviation
# outcomeName - "heart attack", "heart failure", “pneumonia”
# num - ranking of a hospital in that state for that outcome, "best" or "worst" or
# or an integer

# returns vector with name of the hospital that has that ranking
rankhospital <- function(state, outcome, num = "best") {
    
    outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    validStates<-unique(outcomes[,7])
    validRank<-c("best", "worst")
    validConditions<-c("heart attack", "heart failure", "pneumonia")
    
    # CHECKING FOR VALID ARGUMENTS
    # will be NA if no match, so FALSE
    if (is.na(match(state,validStates))) {
        stop("invalid state")
    }

    # will be NA if no match, so FALSE
    if (is.na(match(outcome,validConditions))) {
        stop("invalid outcome")
    }
    
    # will be NA if no match, so FALSE
    if (is.na(match(num,validRank)) && !is.numeric(num) ) {
        stop("invalid rank")
    }

    # 30 Day Death Rates are stored in the following rows:
    # heart attack - 11
    # heart failure - 17
    # pneumonia - 23

    col<-NA
    if (outcome == "heart attack") {
        col<-11
    }
    else if (outcome == "heart failure") {
        col<-17
    }
    else if (outcome == "pneumonia") {
        col<-23
    }

    # first subset based on state
    outcomesByState<-subset(outcomes,subset=outcomes[,7]==state)
    
#    if (num > length(outcomesByState[,2])) {
#        return NA
#    }
    
    hospitalList<-outcomesByState[,c(2,col)]

    newHospitalList<-hospitalList[order(as.numeric(hospitalList[,2]),hospitalList[,1]),]

    if (num == "best") {
        num<-1
    }
    else if (num == "worst") {
        # count the length of non NAs
        num<-length(which(!is.na(as.numeric(newHospitalList[,2]))))
    }
    newHospitalList[num,1]

    # sort the ratings vector
    #ratingsVec <- order(outcomesByState[,col],outcomesByState[,2])
    #ratingsVec

    # get the value at "num"
    #rating <- ratingsVec[num]
    
    #find that rating in the original vector
    #hospital<-subset(outcomesByState,subset=outcomesByState[,col]==rating)
    
    #hospital <- sort(hospitals[,2])
    #hospital[1]
    #ordered<-order(outcomesByState[,col],outcomesByState[,2])
    #binded<-cbind(outcomesByState[,2],ordered)
    #hospital<-subset(binded,subset=binded[,2]==num)
    
    
    
    
}