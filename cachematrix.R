## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##          If the inverse has already been calculated (and the matrix has not changed),
##          then the cachesolve should retrieve the inverse from the cache.

## Creates a special "matrix" object that can cache its inverse.
## Return a list containing a function to
## set matrix, get matrix, set the inverse matrix, get the inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a inverse matrix that is in the list 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    # calculate inverse matrix
    m <- solve(data, ...)
    #
    x$setinverse(m)
    m
}
