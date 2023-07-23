## Put comments here that give an overall description of what your
## functions do

## Create a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # Initialize an empty variable to store the cached inverse
  inv <- NULL

  # Function to set the matrix value
  set <- function(y) {
    x <<- y
    # When the matrix changes, invalidate the cached inverse
    inv <<- NULL
  }

  # Function to get the matrix value
  get <- function() x

  # Function to compute and cache the inverse
  setInverse <- function() {
    inv <<- solve(x)
  }

  # Function to retrieve the cached inverse
  getInverse <- function() inv

  # Return a list of functions to interact with the matrix object
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Compute the inverse of the special "matrix" returned by makeCacheMatrix.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # Check if the cached inverse exists, if so, retrieve and return it
  if (!is.null(x$getInverse())) {
    message("Getting cached inverse.")
    return(x$getInverse())
  }

  # If the cached inverse doesn't exist, compute it and cache it
  inv <- solve(x$get())
  x$setInverse(inv)

  # Return the computed inverse
  inv
}
