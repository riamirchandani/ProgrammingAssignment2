# makeCachedMatrix is a function that returns a list of functions
# Its puspose is to store a martix and a cached value of the inverse of the 
# matrix. Contains the following functions:
# * setMatrix      set the value of a matrix
# * getMatrix      get the value of a matrix
# * cachedInverse  get the cahced value (inverse of the matrix)
# * getInverse     get the cahced value (inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {
  
  # holds the cached value
  # NULL if nothing is cached
  # initially it is set to NULL
  c<-NULL
  
  # store a matrix
  setMatrix <- function(new_val) {
    x <<- new_val
    # since the matrix is assigned a new value, delete the cached
    c <<- NULL
  }
  
  # returns the stored matrix
  getMatrix <- function() x
  
  # c the given argument 
  cachedInverse <- function(solve) {
    c <<- solve
  }
  
  # get the cached value
  getInverse <- function() c
  
  # return a list. Each named element of the list is a function
  list(setMatrix = setMatrix, getMatrix = getMatrix, cachedInverse = cachedInverse, getInverse = getInverse)
}


# The following function calculates the inverse of a matrix which will be using func. makeCachedMatrix
cacheSolve <- function(x, ...) {
  # get the cached value
  inverse <- x$getInverse()
  # if a cached value exists then return it
  if(!is.null(inverse)) {
    message("Getting the cached data. Wait")
    return(inverse)
  }
  # otherwise get the matrix, caclulate the inverse and store it in cache
  data <- x$getMatrix()
  inverse <- solve(data)
  x$cachedInverse(inverse)
  
  # return the inverse
  inverse
}