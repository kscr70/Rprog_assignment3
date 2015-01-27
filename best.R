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
        
<<<<<<< HEAD
}
=======
        By_state <<- state_split[[state]]                
        
        if (outcome == "heart attack") {
                out <- By_state$Hospital.Name[which.min(By_state[,11])]
                
                sort_out <- sort(out)
                print(sort_out[1])
        }
        if (outcome == "heart failure"){
                out2 <- By_state$Hospital.Name[which.min(By_state[ ,17])]
                
                sort_out2 <- sort(out2)
                print(sort_out2[1])
        }
        if (outcome == "pneumonia"){
                out3 <- By_state$Hospital.Name[which.min(By_state[ ,23])]
                sort_out3 <- sort(out3)
                print(sort_out3[1])
                
        }
}
>>>>>>> 8f2f7912d515e3174fecf69b594e3631e0d1ccdb
