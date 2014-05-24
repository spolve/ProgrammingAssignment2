## This functions cache the inversion of a matrix: the first function create a
#special "cahced matrix", the second one copute the result or use the cached value.

## makeCacheMatrix: This function creates a special "matrix" object that can 
#cache its inverse; returns a list containing a function to:
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of matrix inversion
#4. get the value og matrix inversion
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y= matrix()) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}



#This function calculates the inverse of the "matrix" created with 
#makeCacheMatrix. If the inverse has already been calculated it gets the inverse
#from the cache.

cacheSolve  <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
    
}
