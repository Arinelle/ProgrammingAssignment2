## a pair of functions allowing the caching of a matrix's inversion

## this function creates a special object to store a matrix and its inverse as cache
## this object takes the form of both matrices and a list of functions to get and set them
## this is storage only and does not do the inversion computation

makeCacheMatrix <- function(x = matrix()) {
    inverse = NULL
    set = function(y) {
        inverse <<- NULL
        x <<- y
    }
    setInverse = function(solved) {
        inverse <<- solved
    }
    get = function() x
    getInverse = function() inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## inverts an invertible matrix passed inside a makeCacheMatrix object
## if the cache already has the computed result (inverted matrix), it just returns it
## otherwise it computes it using solve, stores it in the cache, then returns it
## additional arguments are passed on to the solve function
## no checks of invertibility are made here, it is supposed that all passed matrices are invertible
## the makeCacheMatrix object must have already had its original matrix set

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse = x$getInverse()
    if(is.null(inverse)) {
        message("computing inverse")
        inverse = solve(x$get(), ...)
        x$setInverse(inverse)
    }
    else {
        message("getting cached data")
    }
    inverse
}
