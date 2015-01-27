rankhospital <- function(state, outcome, num = "best"){
        ## Read outcome data
        library(stats)
        outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        valid_outcome <- c("heart attack", "heart failure", "pneumonia")
        ## Check that state and outcome are valid
        if ((!is.element (state, outcome_data [, 7])) == TRUE) {  #checks whether the state entered is in the state column
                stop("invalid state")
        }
        
        if ((!is.element (outcome, valid_outcome)) == TRUE) {    #checks whether outcome entered is valid
                stop("invalid outcome")
        }
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        state_split <- split(outcome_data, outcome_data$State)
        
        By_state <- state_split[[state]]                
        
        if (outcome == "heart attack") {
                rate <- (By_state[[11]])
                Hosp_name <- (By_state[[2]])
                sort_table <<- (cbind(Hosp_name, rate)[,order(rate,Hosp_name)])
                return(sort_table)
        }
        if (outcome == "heart failure"){
                out2 <- as.character(By_state$Hospital.Name[which.min(By_state[ ,17])])
                
                sort_out2 <- sort(out2)
                return(sort_out2[1])
        }
        if (outcome == "pneumonia"){
                out3 <- as.character(By_state$Hospital.Name[which.min(By_state[ ,23])])
                sort_out3 <- sort(out3)
                return(sort_out3[1])
                
        }
}