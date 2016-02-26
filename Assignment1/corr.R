corr <- function(directory, treshold = 0) {
        
        files <- list.files(directory)
        
        vector <- c()
        for(i in 1:length(list.files(directory))) {
                
                # Creating path and read file
                file <- files[i]
                path <- paste(directory,"/",file, sep="")
                data <- read.csv(path)
                
                # Clearing NAs
                good <- complete.cases(data)
                data <- data[good, ]
                
                if(nrow(data) > treshold) {
                        vector <- c(vector, cor(data[["sulfate"]], data[["nitrate"]]))
                }
                
        }
        vector
}