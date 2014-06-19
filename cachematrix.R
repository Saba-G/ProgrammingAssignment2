## Put comments here that give an overall description of what your
## functions do:

## Functions cache the inverse of a matrix.
## First a list of functions is made in which the inverse of a matrix is calculated 
## and can be called upon once calculated (getinv).
## Then if the inverse of a certain matrix is already calculated before
## the inverse will not be calculated again but will be returned from the cache.

## Write a short comment describing this function:

## First sets the value of m, so if a new matrix is assigned (by ..$set()) it will (re)set this value in the function
## Calling get() will retreive the matrix created
## setinv() is a function that will calculate the inverse of the matrix
## getinv() is a function that will retreive the inverse of the matrix
## So this function makes a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}

## Write a short comment describing this function

## This function will retreive the inverse of the matrix from cache IF inverse of matrix is already calculated
## if the inverse is not yet calculated it calculates it (m <- solve(data,...)) and saves it in cache (x$setinv(m))

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
