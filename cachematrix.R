## Writing two functions: "makeCacheMatrix" & "cacheSolve"
## in order to calculate the inverse of a matrix and also 
## to cache the result, if it's a new value and obtain the
## inverse from cached data if it's been calculated already.

## This function creates a special "matrix" object that can 
## cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
    my_inv <- NULL
    set <- function(y) {
      x <<- y
      my_inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) {
        message("Caching the data") 
        my_inv <<- inverse
    }
    getinv <- function() my_inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    my_inv <- x$getinv()
    if(!is.null(my_inv)){
      message("Data available... Getting cached data")
      return(my_inv)
    }
    data <- x$get()
    my_inv <- solve(data, ...)
    x$setinv(my_inv)
    my_inv
}
