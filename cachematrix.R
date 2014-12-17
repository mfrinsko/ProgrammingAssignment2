## The functions below create a matrix object that can cache its inverse, compute the 
## inverse for that matrix, and return the inverse from the cache on subsequent calls.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL 
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)

}


## This function will compute the inverse of the matrix created in the previous function
## and cache the results the first time it is run.  If the same matrix is passed through
## this function a 2nd time and nothing in the matrix has changed since the first run,
## the inverse will be pulled from the cache.  Otherwise it will recalculate the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if (!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
