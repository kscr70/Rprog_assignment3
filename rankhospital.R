rankhospital <- function(state, outcome, num = "best"){
        ## Read outcome data
       
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
        
        By_state <<- state_split[[state]] 
        if (num == "best"){               # if num is "best" sets num to 1
                num <- 1
        }else{                            # else num is entered number
                num <- num
        }
        
        
        if (outcome == "heart attack") {
                outcome_HA <- as.numeric(By_state[, 11]) #creates numeric vector from 30 day heart attack mortality rates
                not_missing <- !is.na(outcome_HA)  # creates a logical vector indicating which hospitals have mortality data
                
                complete <<- By_state[not_missing, ] #creates data frame without missing data for heart attack mortality rates
                subset_heartattack <<- complete[ , c(2, 7, 11)] #creates a data frame with only columns of interest
                
                rate <- as.numeric(subset_heartattack [, 3]) #creates a numeric vector from 30 day mortality rate
                HospName <- as.character(subset_heartattack [ ,1]) #creates a character vector with hospital names
                
                Hosp_rank <<- rank(rate, ties.method = "min") #ranks hospitals based on mortality rate
                ranked_heartattack <<- cbind(HospName, rate, Hosp_rank) #appends data frame with hospital ranks
                Sort_by_rank_name <<- ranked_heartattack[order(Hosp_rank, HospName), ] #sorts ranked hospitals by name
                
                if (num == "worst"){  # if num is "worst" returns number of last ranked hospital
                        num <- as.numeric(length(Sort_by_rank_name[, 1]))
                }
                
                if (num > length(Sort_by_rank_name[, 1])){ # if num entered is larger than
                        out <- `is.na<-`(NA)               # number of ranked hospitals in given state
                        return(out)                           
                        
                }
                
                final_sort <- c(1:length(Sort_by_rank_name[, 1])) #creates numeric vector where ties are orderd alphabetically
                Sort_by_rank_name_order <<- data.frame(cbind(Sort_by_rank_name, final_sort)) #appends to previous data frame 
                        
                out <- (as.character(Sort_by_rank_name_order$HospName[which(Sort_by_rank_name_order[ ,4] == num)])) # returns name of hospital of a given rank
                return(out)
               
                
                
        }
        if (outcome == "heart failure") {
                outcome_HF <- as.numeric(By_state[, 17]) #creates numeric vector from 30 day heart failure mortality rates
                not_missing <- !is.na(outcome_HF)  # creates a logical vector indicating which hospitals have mortality data
                
                complete <<- By_state[not_missing, ] #creates data frame without missing data for heart attack mortality rates
                subset_heartattack <<- complete[ , c(2, 7, 17)] #creates a data frame with only columns of interest
                
                rate <- as.numeric(subset_heartattack [, 3]) #creates a numeric vector from 30 day mortality rate
                HospName <- as.character(subset_heartattack [ ,1]) #creates a character vector with hospital names
                
                Hosp_rank <<- rank(rate, ties.method = "min") #ranks hospitals based on mortality rate
                ranked_heartattack <<- cbind(HospName, rate, Hosp_rank) #appends data frame with hospital ranks
                Sort_by_rank_name <<- ranked_heartattack[order(Hosp_rank, HospName), ] #sorts ranked hospitals by name
                
                if (num == "worst"){  # if num is "worst" returns number of last ranked hospital
                        num <- as.numeric(length(Sort_by_rank_name[, 1]))
                }
                
                if (num > length(Sort_by_rank_name[, 1])){ # if num entered is larger than
                        out <- `is.na<-`(NA)               # number of ranked hospitals in given state
                        return(out)                        # returns NA and ends function   
                        
                }
                
                final_sort <- c(1:length(Sort_by_rank_name[, 1])) #creates numeric vector where ties are orderd alphabetically
                Sort_by_rank_name_order <<- data.frame(cbind(Sort_by_rank_name, final_sort)) #appends to previous data frame 
                
                out <- as.character(Sort_by_rank_name_order$HospName[which(Sort_by_rank_name_order[ ,4] == num)]) # returns name of hospital of a given rank
                return(out)
        }
        if (outcome == "pneumonia") {
                outcome_Pn <- as.numeric(By_state[, 23]) #creates numeric vector from 30 day heart failure mortality rates
                not_missing <- !is.na(outcome_Pn)  # creates a logical vector indicating which hospitals have mortality data
                
                complete <<- By_state[not_missing, ] #creates data frame without missing data for heart attack mortality rates
                subset_heartattack <<- complete[ , c(2, 7, 23)] #creates a data frame with only columns of interest
                
                rate <- as.numeric(subset_heartattack [, 3]) #creates a numeric vector from 30 day mortality rate
                HospName <- as.character(subset_heartattack [ ,1]) #creates a character vector with hospital names
                
                Hosp_rank <<- rank(rate, ties.method = "min") #ranks hospitals based on mortality rate
                ranked_heartattack <<- cbind(HospName, rate, Hosp_rank) #appends data frame with hospital ranks
                Sort_by_rank_name <<- ranked_heartattack[order(Hosp_rank, HospName), ] #sorts ranked hospitals by name
                
                if (num == "worst"){  # if num is "worst" returns number of last ranked hospital
                        num <<- as.numeric(length(Sort_by_rank_name[, 1]))
                }
                
                if (num > length(Sort_by_rank_name[, 1])){ # if num entered is larger than
                        out <- `is.na<-`(NA)               # number of ranked hospitals in given state
                        return(out)                        # returns NA and ends function   
                        
                }
                
                final_sort <- c(1:length(Sort_by_rank_name[, 1])) #creates numeric vector where ties are orderd alphabetically
                Sort_by_rank_name_order <<- data.frame(cbind(Sort_by_rank_name, final_sort)) #appends to previous data frame 
                
                out <- as.character(Sort_by_rank_name_order$HospName[which(Sort_by_rank_name_order[ ,4] == num)]) # returns name of hospital of a given rank
                return(out)
                
        }
}