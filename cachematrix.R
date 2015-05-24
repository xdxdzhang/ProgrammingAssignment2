## To cache and compute, two functions have been wrotten.

## This function creates a special "matrix" object
## that can cache its inverse.
makeCacheMatrix <- function(max = matrix()) {
    inversemax <- NULL
    set <- function(x) {
        max <<- x;
        inversemax <<- NULL;
    }
    get <- function() 
    return(max);
    setinv <- function(inv) 
    inversemax <<- inv;
    getinv <- function() 
    return(inversemax);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix function above.  
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(max, ...) {
    inversemax <- max$getinv()
    if(!is.null(inversemax)) {
        message("Getting Cached Inverse Matrix Data...")
        return(inversemax)
    }
    data <- max$get()
    invserse_max <- solve(data, ...)
    max$setinv(inversemax)
    return(inversemax)
}
