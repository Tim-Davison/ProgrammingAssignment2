## These functions will create a special matrix and its inversion.
## It will cache the result, so that if it is run again with the
## same matrix, it will give the cached result instead of running
## the computation again.

## The makeCacheMatrix function will create the special matrix and
## run its inverse. It will cache the result.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
      x <<- y
      inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse 
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The cacheSolve function will look to see if the matrix enetered 
## already been created, and inverted. If so, it will provide the
## cached results. If not it will calculate the new matrix and inverse.

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setinverse(inv)
      inv
}
