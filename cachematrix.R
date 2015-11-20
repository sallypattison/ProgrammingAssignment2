## Pair of functions which cache the inverse of a matrix. 


## "makeCacheMatrix" is a function which takes a matrix as its argument and 
## creates a special "matrix" object that stores the input matrix and caches its 
## inverse. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
                }
        get <- function() x
        set_inverse <- function(inverse) inv <<- inverse  
        get_inverse <- function() inv
        list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## "cacheSolve" computes the inverse of the special "matrix" returned by 
##"makeCacheMatrix". If the inverse has already been calculated and the matrix 
## hasn't been changed, then "cacheSolve" will return the message "getting 
## cached data" and retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$get_inverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$set_inverse(inv)
        inv
}
