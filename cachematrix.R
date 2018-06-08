## These functions calculate and cache the inverse of the matrix
## so they can be easily accessed.

## Creates an object for a matrix that can cache its inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set , get = get , setinv = setinv , getinv = getinv)
}



## Computes the inverse matrix returned by make CacheMatrix.
## If it already has been calculated, the inverse is returned.

cacheSolve <- function(x, ...) {
       
        
        inv <- x$getinv()
        if(!is.null(inv) ) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
