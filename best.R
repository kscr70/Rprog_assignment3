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
        state_split <<- split(outcome_data, outcome_data$State)
        
        By_state <<- state_split[[state]]                
        
        if (outcome == "heart attack") {
                print(By_state$Hospital.Name[By_state [ ,11] == min(By_state [ ,11])])
        }
        if (outcome == "heart failure"){
                print(By_state$Hospital.Name[By_state [ ,17] == min(By_state [ ,17])])
        }
        if (outcome == "pneumonia"){
                print(By_state$Hospital.Name[By_state [ ,23] == min(By_state [ ,23])])
        }
}