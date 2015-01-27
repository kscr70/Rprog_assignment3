best <- function(state, outcome){
        ## Read outcome data
        outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        valid_outcome <- c("heart attack", "heart failure", "pneumonia")
        ## Check that state and outcome are valid
        if ((!is.element (state, outcome_data [, 7])) == TRUE) {  #checks whether the state entered is in the state column
                stop("invalid state")
        }else{
                print(state)
        }
        
        if ((!is.element (outcome, valid_outcome)) == TRUE) {    #checks whether outcome entered is valid
                stop("invalid outcome")
        } else {
                print(outcome)
        }
        ## Return hospital name in that state with lowest 30-day death rate
        
}
