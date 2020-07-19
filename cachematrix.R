# Matrix to be used
basicMatrix <- matrix(1:4, nrow= 2, ncol = 2)
useMatrix <- makeCacheMatrix(basicMatrix)

# Make an object that stores the value of the matrix and its inverse to be used
makeCacheMatrix <- function(x = numeric()) {
        inv <- NULL
        #Set the value of the matrix
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        #Get the value of the matrix
        get <- function() (x)
        #Set the value of the inverse of the matrix
        setInverse <- function(inverse) (inv <<- inverse)
        #Get the value of the inverse of the matrix
        getInverse <- function() (inv)
        #List of the function components
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Function that checks if the matrix was cached correctly
cacheSolve <- function(x, ...){
        inv <- x$getInverse()
        # Condition that checks if the inverse matrix has already been cached
        if(!is.null(inv)){
                # And return that message if it's true
                message("Getting cached matrix")
                # If it's false make again the calculation
                return(inv)
        }
        mat <- x$get()
        # With this function
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
        
}
