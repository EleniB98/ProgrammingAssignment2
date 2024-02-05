## Create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the cache for storing the inverse
  cache <- NULL
  
  # Function to set the matrix in the cache
  setMatrix <- function(inputMatrix) {
    x <<- inputMatrix
    cache <<- NULL  # Clear the cache when the matrix is updated
  }
  
  # Function to get the matrix from the cache
  getMatrix <- function() x
  
  # Function to set the cached inverse of the matrix
  setInverse <- function(inverse) {
    cache <<- inverse
  }
  
  # Function to get the cached inverse of the matrix
  getInverse <- function() cache
  
  # Return a list of functions
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Compute the inverse of a matrix, caching the result for future use
cacheSolve <- function(x, ...) {
  # Get the cached inverse from the cache
  cachedInverse <- x$getInverse()
  
  # If the cached inverse is available, return it
  if (!is.null(cachedInverse)) {
    message("getting cached inverse")
    return(cachedInverse)
  }
  
  # If not, compute the inverse using solve function
  matrixData <- x$getMatrix()
  inverse <- solve(matrixData, ...)
  
  # Set the computed inverse in the cache
  x$setInverse(inverse)
  
  # Return the computed inverse
  inverse
}

