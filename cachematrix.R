## These are pair of functions that cache the inverse of a matrix.

## makeCacheMatrix is a function that returns a list of functions
# Its puspose is to store a martix and a cached value of the inverse of the 
# matrix. Contains the following functions:
# * setMatrix      set the value of a matrix
# * getMatrix      get the value of a matrix
# * cacheInverse   get the cahced value (inverse of the matrix)
# * getInverse     get the cahced value (inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) 
{
    inv_cache <- NULL
    set <- function(y) 
    {
        x <<- y
        inv_cache <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv_cache <<- inverse
    getinv <- function() inv_cache
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#  If the inverse has already been calculated (and the matrix has not changed), 
#  then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
    inv_cache <- x$getinv()
    if(!is.null(inv_cache)) 
    {
        message("Getting cached data")
        return(inv_cache)
    }
    data <- x$get()
    inv_cache <- solve(data, ...)
    x$setinv(inv_cache)
    inv_cache
}
