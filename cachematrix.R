## Solution to Assignment 2 :: Caching the Inverse of a Matrix

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inverse <<- inv
  getinv <- function() inverse
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}
# The cacheSolve function returns the inverse of the matrix. It firstly checks if
# the inverse has already been computed. If inverse is already computed, it gets the result and leaves the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.


cacheSolve <- function(x) {
  inverse <- x$getinv()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinv(inverse)
  inverse
}