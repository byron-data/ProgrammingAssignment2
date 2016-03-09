## Caching the Inverse of a Matrix

## Write a pair of functions that cache the inverse of a matrix rather
## than computing it repeatedly thus saving a costly computation.

## This function creates a matrix of class type list and
## also stores a value for the inverse of the matrix.
## Included are functions to: set and get the value of the matrix;
## set and get the inverse of the matrix;

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function calculates the inverse of the matrix as created by the
## makeCacheMatrix function. Before calculating the inverse of the
## matrix it checks to see if it has already been calculated by using
## the getInverse function, if there is no value it calculates it and
## stores it using the setInverse function to save further computations.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
