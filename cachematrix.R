## These functions are used to cache the inverse of a matrix.
## The purpose is to avoid the same computations again and again
##  of matrix inversions by caching the result.

## `makeCacheMatrix` function creates a special "matrix" object that can cache its inverse.
## It returns a list of functions to:
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse

# Function `makeCacheMatrix`
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" object created 
## by `makeCacheMatrix`. If the inverse has already been calculated and the matrix 
## has not changed, it retrieves the cached inverse to save computation time.
## Otherwise, it calculates the inverse and stores it in the cache.

# Function `cacheSolve`
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
