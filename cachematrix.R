## This function creates a special "matrix" object that can cache its inverse

## It holds 4 functions to look up or to store a square matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    ## Function 1: Sets new matrix up for calculation 
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    ## Function 2: Retrieves current matrix
    get <- function() x
    ## Function 3: Places inverse of matrix into cache
    setinverse <- function(inversematrix) inverse <<- inversematrix
    ## Function 4: Retrieves inverse of matrix from cache
    getinverse <- function() inverse
    ## Avails 4 functions for use
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    ## If the inverse has already been calculated and the matrix has not changed,
    ## then cacheSolve retrieves the inverse from the cache
    if(!is.null(inverse)) {
        message("Getting cached data")
        return(inverse)
    }
    ## Otherwise, it calculates the inverse of the new matrix
    data <- x$get()
    inverse <- solve(data)
    ## and stores its inverse in the cache
    x$setinverse(inverse)
    inverse
}