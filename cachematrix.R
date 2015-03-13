# This file containts 2 functions:
# - makeCacheMatrix() creates a special matrix able to cache its inverse.
# - cacheSolve() returns the inverse of a matrix created with 
#   makeCacheMatrix() and chaches the inverse so it does not have 
#   to be recomputed the subsequent times.
#
# Example of use:
#   > m = matrix(rnorm(1000000), nrow=1000, ncol=1000)
#   > sol <- solve(m)
#   > cm <- makeCacheMatrix(m)
#   > csol <- cacheSolve(cm)
#   > csol <- cacheSolve(cm)
#   getting cached data
#   >identical(sol, csol)
#   [1] TRUE


# Returns a special matrix able to cache its inverse.
# Arguments: 
#   x - original matrix.
# Returns: 
#   A list with 4 functions to get (get()) and set (set())
# the original matrix and to cache (setInverse()) and retrieve 
# (getInverse()) the cached inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # To store the inverse of x
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) m <<- inv
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# Retrieves the inverse of a matrix x. The inverse of the matrix
# is computed only the first time and it is stored so it does not
# have to be recomputed the subsequent times.
# Arguments:
#   x - special matrix created with makeCacheMatrix().
# Returns:
#   The inverse of x.
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # It is the first time, so compute and store
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
