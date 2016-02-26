best <- function(state, outcome) {
        
        ## Read outcome data for given state
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state is valid, keep only data for given state
        if(!any(data[,7] == state)) stop("invalid state")
        data <- split(data, data[,7])[state][[1]]
        
        ## Check that outcome is valid, strip unrelevant columns
        if(outcome == "heart attack") data <- data[,c(2,11)]       
        else if (outcome == "heart failure") data <- data[,c(2,17)]       
        else if (outcome == "pneumonia") data <- data[,c(2,23)]       
        else stop("invalid outcome")
        
        ## Strip not available data rows, create numeric data
        bools <- data[,2] != "Not Available"
        data <- data[bools, ]
        data[,2] <- as.numeric(data[,2])
        
        ## Return hospital names with lowest 30-day death rate
        data <- data[order(data[,2],data[,1]),]
        data[1,1]
}