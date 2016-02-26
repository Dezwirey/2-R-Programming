rankall <- function(outcome, num = "best") {
        
        ## Read outcome data for given state
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that outcome is valid, strip unrelevant columns
        if(outcome == "heart attack") data <- data[,c(2,7,11)]
        else if (outcome == "heart failure") data <- data[,c(2,7,17)]
        else if (outcome == "pneumonia") data <- data[,c(2,7,23)]       
        else stop("invalid outcome")
        
        ## Strip not available data rows, create numeric data
        bools <- data[,3] != "Not Available"
        data <- data[bools, ]
        data[,3] <- as.numeric(data[,3])

        data <- split(data, data[,2])

        ## Sorting per state, keep winner
        for(i in 1:length(data)){
                x <- data[[i]]
                x <- x[order(x[,3],x[,1]),]

                if(is.numeric(num)) row <- num
                else if (num == "best") row <- 1
                else if (num == "worst") row <- nrow(x)
                x <- c(x[row,1],x[row,2])
                data[i] <- x
        }       
        as.data.frame(unlist(data))
}