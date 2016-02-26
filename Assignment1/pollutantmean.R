pollutantmean <- function(directory, pollutant, id = 1:332) {
        
        files <- list.files(directory)
        vector = c()
        
        for(i in id) {
                
                # Creating path and read file
                file <- files[i]
                path <- paste(directory,"/",file, sep="")
                data <- read.csv(path)
                
                # Extracting column
                data <- data[pollutant]
                
                # Clearing NAs
                good <- !is.na(data)
                data <- data[good]
                
                # Adding to vector 
                vector = c(vector, data)
        }
        mean(vector)
}