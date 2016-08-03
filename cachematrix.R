## Put comments here that give an overall description of what your
## functions do

## This function is called for inversing matrix using cache due to its costly computation.
## Once the inverse of matrix is computed, it will be stored in cache 
## and we can retrieve the inversion of matrix in cache rather than computing it again.

## Write a short comment describing this function
## This function is called for creating a list of functions the will be used in 'cachesolve' function for caching.

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## Write a short comment describing this function
## This function is called for storing new inverse of matrix 
## and returning the inversion value if it already exists in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        dat <- x$get()
        m <- solve(dat, ...)
        x$setinv(m)
        m
}
