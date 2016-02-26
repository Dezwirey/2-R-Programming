## These functions show how lexical scoping works. The variables i and x are declared inside 
## the functions but keep their values outside of them. 


## The first function provides 'toolbox' in a list a matrix and its inverse - i.e. getting 
## and setting both a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(y) i <<- y
        getinverse <- function() i
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)      
        
}


## The second function makes use of these tools to actually create the inverse. First, i is 
## set to the inverse which probably isn't yet calculated (31). If it has already, it is 
## being loaded from the 'toolbox' (33-34). Then the given matrix is being loaded, after 
## which its inverse is being calculated and set to i (39-41).

cacheSolve <- function(x, ...) {
        
        i <- x$getinverse()
        
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}   