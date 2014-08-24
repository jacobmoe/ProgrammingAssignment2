## Solves and caches the inverse of a matrix.
## If the inverse already exists in the cache,
## cacheSolve returns the cached inverse.
## If the inverse is not in the cache,
## cacheSolve will solve the inverse and store it in the cache
## Setting a new matrix will invalidate the cache. 

## Caches the inverse of a matrix.
## Takes a solvable matrix as the sole argument.
## Returns 4 functions: set, get, setinverse, getinverse.
## Used as an argument for the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL

    set <- function(y) {
        x <<- y
        i <<- NULL
    }

    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i

    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Solves or returns the cached inverse of a matrix.
## Takes a cacheMatrix object as an argument.
## Also takes options to be passed to the solve function. 
## Used in conjunction with makeCacheMatrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    i <- x$getinverse()
    if(!is.null(i)) {
            message("getting cached data")
            return(i)
    }

    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
