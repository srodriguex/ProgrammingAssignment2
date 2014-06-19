## This script defines two special functions to cache the inverse of a matrix.
## This is recommended when you are working with large matrix.

## Creates an object that simply stores a matrix and its inverse.
## Use the setters and getters to write and read them.
## 'x': a matrix object.
makeCacheMatrix <- function(x = matrix()) {
  # Local variable to store the inverse of 'x'.
  m <- NULL
  
  # Store the matrix passed in 'y'.
  set <- function(y) {
    
    # Store the matrix in the variable 'x' residing in the outer environment.
    x <<- y
    
    # As resulting of storing a new matrix, sets its inverse to NULL in order
    ## to flag it to be calculated later on.
    m <<- NULL
  }
  
  # Return the matrix stored in 'x'.
  get <- function() x
  
  # Store the matrix in 'inverse' as the inverse of 'x'. Note that someone
  # will  be responsible to correctly calculate the inverse of 'x'.
  setinverse <- function(inverse) m <<- inverse
  
  # Return the inverse of 'x' stored in 'm'.
  getinverse <- function() m
  
  # Definition of the setters and getters.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Calculates the invertible matrix stored in 'x' created by 
## the function 'makeCacheMatrix' if it hasn't been calculated yet.
cacheSolve <- function(x, ...) {
  # Read the inverse matrix stored in 'x'.
  m <- x$getinverse()
  
  # If it has been calculated, return it.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # Or maybe it hasn't been calculated yet, so do it now.
  data <- x$get()
  m <- solve(data, ...)
  
  # Store it in the special object 'x' and return.
  x$setinverse(m)
  m  
}
