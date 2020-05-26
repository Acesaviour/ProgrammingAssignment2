## The combination of the following 2 functions help to cache the inverse of a matrix
## This is to reduce unnecessary repetitive recomputation


## This function creates a special "matrix" object that can cache its inverse
## Essentially creating an output of of list of four functions (set, get, setInverse and getInverse)

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


## This function computes or retrives the inverse of the special "matrix" created by makeCacheMatrix above. If the inverse has already been calculated (and the 
## If the matrix has not changed, then it should retrieve the inverse from the cache.
## It will only recompute if the matrix has changed.

cacheSolve <- function(x, ...) {
        
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}