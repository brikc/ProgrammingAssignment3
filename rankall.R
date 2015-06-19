
rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    
    ## Check that state and outcome are valid
    validRank<-c("best", "worst")
    validConditions<-c("heart attack", "heart failure", "pneumonia")
    
    # check outcomes - will be NA if no match, so FALSE
    if (is.na(match(outcome,validConditions))) {
        stop("invalid outcome")
    }
    
    # check num - will be NA if no match, so FALSE
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
    
    ## for each state, find the hospital of the given rank
    statesList<-unique(outcomes[,7])
    
    # now alphabetize statesList
    statesList<-sort(statesList)
    
    # these are the vectors that we will write to a dataframe
    hospitalVector <- NULL
    stateVector <- NULL
    
    for (state in statesList) {
        
        # first subset based on state
        outcomesByState<-subset(outcomes,subset=outcomes[,7]==state)
        
        # next get just the hospital name and the outcome data
        hospitalList<-outcomesByState[,c(2,col)]
        
        # next order this data 
        newHospitalList<-hospitalList[order(as.numeric(hospitalList[,2]),hospitalList[,1]),]
        
        # handle the "best" and "worst" arguments, don't overwrite num
        if (num == "best") {
            numIndex<-1
        }
        else if (num == "worst") {
            # count the length of non NAs
            numIndex<-length(which(!is.na(as.numeric(newHospitalList[,2]))))
        }
        else {
            numIndex<-num
        }
        
        #print(numIndex)
        # this is the hospital that should be added to the dataframe
        hospitalVector<-c(hospitalVector,newHospitalList[numIndex,1])
        stateVector<-c(stateVector,state)
        
    }
    
    
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    df<-data.frame(hospitalVector,stateVector)
    colnames(df) <- c("hospital", "state")
    df

    
}