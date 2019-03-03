## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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

## This function is to calculate the 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    my_inv <- x$getinv()
    if(!is.null(my_inv)){
      message("getting cached data")
      return(my_inv)
    }
    data <- x$get()
    my_inv <- solve(data, ...)
    x$setinv(my_inv)
    my_inv
}
