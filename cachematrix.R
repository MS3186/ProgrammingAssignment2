## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The only change made to the cachevector code, is to replace in the cacheSolve function the mean by the inverse
## To do so, solve funcion is used

# makeCacheMatrix create a vector to be used in cachesolve matrix
# makeCacheMatrix is a set of setter and getter functions

makeCacheMatrix <- function(x = matrix()) {
        # initialisation of inverse
        m <- NULL
        # caching of values
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        # definition of set and get functions for inverse
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        ## definition of output list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)        

}

## CacheSolve return the inverse of the matrix using as input the output of makeCacheMatrix to use caching

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' first using caching data
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## if nothing cached, computes the result, display and stores it for further use
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
