## Programming Assignment 2: Caching the Inverse of a Matrix
## This file contains two functions:
## 1. makeCacheMatrix: creates a special matrix object that can cache its inverse
## 2. cacheSolve: computes the inverse of the special matrix returned by makeCacheMatrix

## makeCacheMatrix: creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # set the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # get the value of the matrix
    get <- function() x
    
    # set the value of the inverse
    setInverse <- function(inverse) inv <<- inverse
    
    # get the value of the inverse
    getInverse <- function() inv
    
    # return a list of functions
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve function retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    
    # if the inverse is already cached, return it
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    
    # otherwise, compute the inverse
    data <- x$get()
    inv <- solve(data, ...)
    
    # cache the inverse for future use
    x$setInverse(inv)
    
    inv
}
