## Create a matrix and input it in a cache then print 
## the inverse of the matrix.

## Creates matrix and inputs data into cache if answer is 
## different.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) inv <<- solve(x)
        getInverse <- function() inv
        list(set =set, get = get,
        setInverse = setInverse, getInverse = getInverse)
}

## Takes the matrix from cache and outputs the inverse.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
