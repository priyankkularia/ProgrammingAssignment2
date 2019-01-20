## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function is making a set of functions and returning them as a list.
## set function is setting values of x and m whenever matrix is changed.
## get function is getting the values of x(matrix)
## setinverse function get the input argument as inverse matrix and set it to m as cache matrix.
## getinverse returns the cached inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(matrixinverse) m <<- matrixinverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Write a short comment describing this function
## This function 1st gets the value of the getinverse and checks whether it is null or not. if it is not(when input matrix is not changed) then is returns the cached inverse matrix.
## Otherwise it gets the new matrix, calculate the inverse and set that inverse as cached inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached inverse matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}