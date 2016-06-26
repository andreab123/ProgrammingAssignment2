## The functions makeCacheMatrix and cacheSolve allow for the inverse of a
## matrix to be calculated, cached and later retrieved.

## This function creates a "matrix" object that can cache its inverse.
## It is assumed that the matrix is a square invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the "matrix" object returned by
## the function makeCacheMatrix. If the inverse has been calculated and
## has not changed, this function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("retrieving cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

