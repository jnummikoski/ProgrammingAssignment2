## The functions contained in this file will create a matrix object, calculate
## the inverse matrix and cache the inverse matrix.

## This function creates a matrix object that is capable of caching its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(solve) i <<- solve
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)        
}


## This function computes the inverse of the matrix provided by
## "makeCacheMatrix" and then caches the inverse.  If the inverse has already
## been cached, it will return the cached inverse.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i      
}
