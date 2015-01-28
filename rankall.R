rankall <- function(outcome, num = "best") {
        ## Read outcome data
        outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        valid_outcome <- c("heart attack", "heart failure", "pneumonia")
        
        ## Check that outcome is valid
                
        if ((!is.element (outcome, valid_outcome)) == TRUE) {    #checks whether outcome entered is valid
                stop("invalid outcome")
        }
        
        ## For each state, find the hospital of the given rank
        state_split <<- split(outcome_data, outcome_data$State) #creates a list of data frames split out by state
        
        if (outcome == "heart attack"){
                outcome_HA <- as.numeric(outcome_data[, 17]) #creates numeric vector from 30 day heart failure mortality rates
                
                not_missing <- !is.na(outcome_HA)  # creates a logical vector indicating which hospitals have mortality data
                
                complete_outcome_HA <<- outcome_data[not_missing, ] #creates data frame without missing data for heart attack mortality rates
                
                
        }
        if (outcome == "heart failure"){
                outcome_HF <- as.numeric(outcome_data[, 17]) #creates numeric vector from 30 day heart failure mortality rates
                
                not_missing <- !is.na(outcome_HF)  # creates a logical vector indicating which hospitals have mortality data
                
                complete_outcome_HF <<- outcome_data[not_missing, ] #creates data frame without missing data for heart attack mortality rates
        }
        
        if (outcome == "pneumonia"){
                outcome_Pn <- as.numeric(outcome_data[, 17]) #creates numeric vector from 30 day heart failure mortality rates
                
                not_missing <- !is.na(outcome_HF)  # creates a logical vector indicating which hospitals have mortality data
                
                complete_outcome_Pn <<- outcome_data[not_missing, ] #creates data frame without missing data for heart attack mortality rates
        }
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
}