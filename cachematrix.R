# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than computing it
# repeatedly.  This file contains a pair of functions that cache the
# inverse of a matrix.

# makeCacheMatrix: This function creates a special "matrix" object that can
# cache its inverse.
#
# ARGS:
#   x: Optional initial matrix
#
# RETURNS:
#   A special "matrix", which is really a list containing the functions
#     set: set the value of the matrix
#     get: get the value of the matrix
#     setinverse: set the value of the inverse
#     getinverse: get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# cacheSolve: This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. If the inverse has already been calculated
# and the matrix has not changed, then cacheSolve retrieves the inverse from
# the cache.
#
# ARGS:
#   x: The special "matrix" returned by makeCacheMatrix.
#   ...: Parameters for the "solve" function
#
# RETURNS:
#   The inverse of the "matrix"

cacheSolve <- function(x, ...) {

    # if inverse exists use it, otherwise solve it
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
    } else {
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
    }

        ## Return a matrix that is the inverse of 'x'
    
    # Shall it return a normal matrix?
    return(i)

    # ... or a special matrix like 'x'?
    # Since the inverse of the inverse is 'x' itself, we could return it as a
    # special inverse-already-cached-no-calculation-needed matrix.
    # special <- makeCacheMatrix(i)
    # special$setinverse(x)
    # return(special)
}
