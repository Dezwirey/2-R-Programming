complete <- function(directory, id = 1:332) {
        
        files <- list.files(directory)
        x <- data.frame()
        
        for(i in id) {
                
                # Creating path and read file
                file <- files[i]
                path <- paste(directory,"/",file, sep="")
                data <- read.csv(path)
                
                # Clearing NAs
                good <- complete.cases(data)
                data <- data[good, ]
                
                # Adding rows to data frame
                newrow <- c(i, nrow(data))
                x <- rbind(x, newrow)
        }
        names(x)[1] = "id"
        names(x)[2] = "nobs"
        x
}