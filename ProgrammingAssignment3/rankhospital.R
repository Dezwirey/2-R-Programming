rankhospital <- function(state, outcome, num = "best") {
        
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
        
        ## Sorting according to lowest death rate, print with ranking num
        data <- data[order(data[,2],data[,1]),]
        if(is.numeric(num)) print(data[num,1])
        else if (num == "best") print(data[1,1])
        else if (num == "worst") print(data[nrow(data),1])
}