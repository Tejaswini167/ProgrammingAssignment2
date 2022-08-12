## The functions makeCacheMatrix and cacheSolve described below are used in generating
## a "cache inverse" for a given matrix.
## This is helpful when the inverse is repeatedly computed,
## Here the value of the inverse is calculated once and is stored in the cache.
## This "cache inverse" is simply accessed whenever the inverse needs to be computed.
## Thus, avoiding any unnecessary recomputations.


## This function creates a special "matrix" object that can cache its inverse.
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


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

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
