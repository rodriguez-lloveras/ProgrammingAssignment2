## the following functions intend to cache and compute
## the inverse of a matrix.

## The first function, makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to:
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse
## 4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y;
        inverse <<- NULL;
    }
    get <- function() return(x);
    setinv <- function(inv) inverse <<- inv;
    getinv <- function() return(inverse);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))

}


## The second function The following function computes the inverse of
## the special "matrix" created with the above function. However, it
## first checks to see if the inverse has already been calculated. If so,
## it gets the inverse from the cache and skips the computation. Otherwise,
## it calculates the inverse of the matrix and sets the value of the inverse in
## the cache via the setinv function

cacheSolve <- function(x, ...) {
inverse <- x$getinv()
    if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
        ## Return the cached matrix that is the inverse of 'x'
    }
    data <- x$get()
    invserse <- solve(data, ...)
    x$setinv(inverse)
    return(inverse)
        ## Return the calculated matrix that is the inverse of 'x'
}
