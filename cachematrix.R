## Programming Assignment Two
##
## Program to cache the inverse of a matrix in order to reduce the
## computational run-time for such a calculation.
##
## NOTE:  I used the code provided by the assignment instructions as a 
##        starting point, and then just modified it to do the inverse rather
##        the mean
##

##
## This function create a special "Matrix" object that can cache its inverse.
##
makeCacheMatrix <- function(x = matrix()) 
{
    m <- NULL
    set <- function(y) 
    {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

##
## This function computes the inverse of the special "matrix" returned by
## the function makeCacheMatrix
##
cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if (!is.null(m)) 
    {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
