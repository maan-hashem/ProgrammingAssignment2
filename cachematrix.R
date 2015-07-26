## makeCacheMatrix is a function that creates a list containing a function to
##
## set the value of the matrix
## get the value of the matrix
## set the value of inverse of the matrix
## get the value of inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
  

## The cachesolve function returns the inverse of the matrix. The first step is to check if
## the inverse has already been computed. If the inverse is available, it gets the result and skips the
## computation. If the inverse is not available, it computes the inverse, sets the value in the cache via
## setinverse function.  This function assumes that the matrix is  invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cashmoney cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}