## Matrix inversion is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than compute
## it repeatedly. The file contains a pair of functions that cache the
## inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # The first function, makeVector creates a special "vector", 
  #   which is really a list containing a function to
  # set the value of the matrix
  # get the value of the matrix
  # set the inverse of the matrix
  # get the inverse of the matrix
  
  s <- NULL
  set <- function(y) { # set the value of the matrix
    x <<- y
    s <<- NULL
  }
  get <- function() x # get the value of the matrix
  setinverse <- function(solve) s <<- solve # set the inverse of the matrix using solve function
  getinverse <- function() s # get the inverse of the matrix
  
  # return a matrix by creating a list and then converting list to matrix
  tmp_list <- list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  as.matrix(tmp_list)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
  ## If the result is already cached, return it
  if (!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  ## If the result it not cached, compute it using solve
  data <- x$get()
  s <- solve(data, ...)
  ## Cache the computed inverse
  x$setinverse(s)
  s
}
