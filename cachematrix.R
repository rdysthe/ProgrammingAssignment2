# The following functions create an object, using the <<- operator, that stores a
# + matrix and caches its inverse
# Per the Assignment instructions, the matrix supplied is assumed to be invertible.

makeCacheMatrix <- function(mx = matrix()) {
  
  # The function makeCacheMatrix creates list of functions that ultimately get the 
  # + inverse of the matrix mx.
  
  # Initialize mxInverse, the inverse of the matrix mx
  mxInverse <- NULL
  
  # Set the value of the matrix when called
  mx_set <- function() {
    mx <<- y
    mxInverse <<- NULL
  }
  
  # Return the value of the matrix mx when called
  mx_get <- function() mx
  
  # Set the value of the inverse of the matrix mx when called
  mxInverse_set <- function(solve) mxInverse <<- solve
  
  # Return the value of the inverse of the matrix mx when called
  mxInverse_get <- function() mxInverse
  
  # Create a list that calls the functions 
  # + mx_set, mx_get, mxInverse_set, and mxInverse_get
  list(mx_set = mx_set, mx_get = mx_get, mxInverse_set = mxInverse_set, mxInverse_get = mxInverse_get)
  
}


cacheSolve <- function(mx, ...) {
  
  # Set the value of mxInverse by setting it to the value calculated in 
  # + makeCacheMatrix
  mxInverse <- mx$mxInverse_get()
  
  # If the value is NOT NULL, then mxInverse has already been calculated. The function 
  # + returns the cached mxInverse value and exits the function.
  if(!is.null(mxInverse)) {
    return(mxInverse)
  }
  
  # If the value returned is NULL, the mxInverse has not yet been calculated, 
  # + so the function now does that.
  
  # Get the value of the matrix mx
  mx_value <- mx$mx_get()
  
  # Calculate and assign the value of mxInverse to the inverse of mx_value, 
  # + which is equivalent to the matrix mx
  mxInverse <- solve(mx_value, ...)
  
  # Cache the calculated inverse
  mx$mxInverse_set(mx)
  
  # Return the value of the inverse
  mxInverse
  
}
