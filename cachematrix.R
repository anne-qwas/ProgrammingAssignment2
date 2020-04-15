## These functions creates a temporary memory position for the calculated
## inverse of a matrix, such that this will not have to be calculated again
## during subsquent actions.

## This function translates your square matrix into a special 'list'
## matrix with the environment needed to enable the temporary memory position.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function either returns the already calculated inverse matrix, or
## calculates the inverse of the given square matrix.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
