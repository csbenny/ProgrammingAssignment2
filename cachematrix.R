## Matrix inversion is an expensive computation as the dimensions getting large
## Makes sense to cache the value of its inverse if the matrix isn't changed
## Function makeCacheMatrix creates a special 'matrix' for caching the inverse
## Function cacheSolve calculates the inverse of a matrix and cache it under special 'matrix'

## Creates a special "matrix" object that cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        
        set <- function(y) {
            x <<- y
            inv_x <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv_x <<- inverse
        getInverse <- function() inv_x
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Retrieve the inverse from the cache if already calculated
## Otherwise, computes the inverse of matrix
## and updates the special 'matrix' for caching the result
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$getInverse()
        if(!is.null(inv_x)) {
            message("getting cached data")
            return(inv_x)
        }
        data <- x$get()
        inv_x <- solve(data, ...)
        x$setInverse(inv_x)
        inv_x
}
