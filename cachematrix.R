## These functions will take an invertible matrix (ie square matrix) and
## calculate the inverse using the R solve function. The result will be
## cached for possible later use.

## This function is used to create a special "matrix" object that 
## caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- matrix(nrow = 0, ncol = 0)
    set <- function(y) {
      x <<- y
      m <<- matrix(ncol=0, nrow = 0)
    }
    get <- function() x
    setinverse <- function(inv) m <<- inv
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse of an unchanged matrix has 
## already been calculated, then cachesolve will obtain the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if (!((ncol(m) == 0) && (nrow(m) == 0))) {
            message("getting cached data")
            ## return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

