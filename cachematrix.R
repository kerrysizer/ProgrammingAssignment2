## Put comments here that give an overall description of what your
## functions do

## Writes the matrix and its inverse to the cache and retrieves the 
## matrix and its inverse from cache

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) im <<- inverse
        getinverse <- function() im
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function first checks to see if the inverse matrix exists.  If so, 
## it gets it from the cache and doesn't create the inverse.  If inverse matrix
## doesn't exist it calculates it and sets it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getinverse()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setinverse(im)
        im
        
}
