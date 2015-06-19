
# state - takes two letter state abbreviation
# outcomeName - "heart attack", "heart failure", “pneumonia”
best <- function(state, outcome){
    
    outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    validStates<-unique(outcomes[,7])
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
    #print(nrow(outcomesByState))
    
    #find the minimum
    minVal<-min(as.numeric(outcomesByState[,col]),na.rm=TRUE)
    #print(minVal)
    
    # get a subset of all the rows that equal minVal
    subset<-subset(outcomesByState,subset=as.numeric(outcomesByState[,col])==minVal)
    
    #print(nrow(subset))
    
    hospitals<-sort(subset[,2])
    hospitals
    
    
}