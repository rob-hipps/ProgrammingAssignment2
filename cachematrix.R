## The function makeCacheMatrix will create a cache for a matrix as it is made. This will
#prevent R from having to potentially loop through the calculation over nd over.
## The cacheSolve function will take the inverse of the created matrix.

## This function will cache a matrix

makeCacheMatrix <- function(x = matrix()) {
    invse = NULL
    set = function(y) {
        x <<- y #assigns this within the environmnet other than here
        invse <<- NULL
    }
    get = function() x
    setinv = function(inverse) invse <<- inverse
    getint = function() inv
}


## This functions calculates the inverse of the cache in makeCacheMatrix

cacheSolve <- function(x, ...) {
        invse = x$getinv()
        
        #This takes care of the situation where the inverse has been calculated
        if(!is.null(invse)) {
            return(invse)
        }
        ans = x$get()
        invse = solve(ans,...)
        
        x$setinv(invse)
        
        return(invse)
}
