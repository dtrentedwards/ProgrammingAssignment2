## The purpose of these two functions is to illustrate how to cache variables
## This is done using the deep assignment variable (<<-) which modifies existing
## variables in the parent environments
## Using the following two functions, we will cache the inverse of matrix x

## The purpose of the makeCacheMatrix is to cache the inverse matrix (m)
## This is done by creating four functions (set, get, setinverse and getinverse)
## The output of this function is a list of functions that will be used in the
## cacheSolve function

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


## The cacheSolve function calls the list of functions generated in the makeCacheMatrix
## checks to see if the matrix inverse has already been created and cached before
## calculating the inverse matrix.  If the inverse has been calculated and cached
## a message stating the function is getting the cached data is presented.  If not
## cacheSolve calculates the inverse matrix.  The inverse of x is m

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m

}
