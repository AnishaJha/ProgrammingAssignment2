## Put comments here that give an overall description of what your
## functions do
## The functions are used to create a matrix, get and set the values of the matrix,  
## get and set the values of inverse. Incase if the inverse is already calculated it
## is cached and it is not recalculated. Incase if the value of matrix is set to a new 
## value the value of inverse is set to NULL.


## Write a short comment describing this function
##
## makeCacheMatrix creates a matrix and has four functions to get and set the values
## of the matrix and its inverse.
## Variables and functions
## x is the matrix
## inv is the inverse.
## get() gets the value of matrix x
## set() sets the value of matrix x
## getinverse() returns the value of inverse
## setinverse() sets the value of inverse

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    print(x)
    print(inv)
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## Write a short comment describing this function
## cacheSolve returns the inverse if it has already been calculated.
## if it has not been calculated it calculates the inverse and sets it in the caller variable.
## 
## Variables
## inv is the inverse of matrix
## data is the matrix on which we do the computation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        print(inv)
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    print(inv)
    inv
    
}
