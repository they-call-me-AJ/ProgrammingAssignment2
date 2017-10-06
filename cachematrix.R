## These two functions can be used together to output the inverse of an 
## inputted matrix.  The inverse of the matrix will be cached, so it can
## be retrieved if the matrix has not been changed.

## This function will create a special "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
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


## This function will return the inverse of matrix "x"
## If the inverse has already been calculated (and the matrix has not changed)
        ## Then the function will retrive the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
