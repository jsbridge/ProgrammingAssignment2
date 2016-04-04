## These functions provide an efficient way to get the inverse of a matrix
## by getting the solution from the cache if the inverse has already been caluclated

## This function creates a matrix object that allows you to cache its inverse

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(matrix) m <<- matrix
    getmatrix <- function() m
    list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
    
}


## This function computes the inverse of the matrix returned by the function
## makeCacheMatrix().  If the inverse has been saved in cache, it grabs it

cacheSolve <- function(x, ...) {

    m <- x$getmatrix()
    if (!is.null(m)) {
        message('Getting cached data')
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}

