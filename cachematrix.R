## These functions provide caching of the inverse of a matrix. Constructs an object containing a matrix
## and provides functions to:
##     set the matrix,
##     set the inverse of the matrix,
##     get the matrix,
##     get the inverse of the matrix
##
## When the matrix is set with a new value,
## the stored inverse is deleted.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates the inverse of a matrix stored in an object
## from the 'makeCacheMatrix' function, and caches the inverse
## in this object. Or, when the inverse is already cached
## in this object, this cached inverse is returned and
## no calculations are done.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
