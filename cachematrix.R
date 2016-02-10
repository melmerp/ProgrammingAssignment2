## The calculation of a matrix inverse can be compute
## intensive. The two functions in this file help
## reduce the cost of matrix inversion by caching
## the inverse of a matrix so it will only need to be
## calculated once as long as the matrix does not
## change

## The first function, makeVector creates a special
## "matrix", which is really a list containing functions:
## . set(y) -- stores a new matrix
## . get() -- gets the stored matrix
## . setinverse(inverse) -- stores the inverse
## . getinverse() -- returns the inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function calculates the inverse of the
## given cacheMatrix (created by makeCacheMatrix()).
## It caches the inverse as long as the matrix
## has not changed.
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    
    ## if i is not null, then the inverse has been
    ## cached so we can return it right away
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## if we get here, then we calculate the inverse,
    ## cache it, and return the result
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
