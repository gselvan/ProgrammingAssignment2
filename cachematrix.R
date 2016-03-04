## Matrix inversion is a costly computation
## Caching the inverse of a Matrixfunctions 

## Caching the inverse of a Matric is beneficial rather than computing it repeateldly
## this function creates a cache matrix

makeCacheMatrix <- function(x = matrix()) {
        m <-NULL
        set <-function(Y){
        x <<-y
        m <<- NULL
        }
        get <-function() x
        setInverse <-function(Inverse) m <<- Inverse
        getInverse <-function() m
        list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## This function creates a function to inverse the cache matirx

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inv <-x$getInverse()
        if(!is.null(Inv){
        message ("getting cached data")
        return(Inv) 
        }
        data <- x$get()
        m <- Inverse(data, ...)
        x$setInverse(Inv)
        Inv
}

