## Functions to save time for calculating the inverse of a given matrix 
# This function is inspired by the example given in the R Course

# Benerink 2015

## makeCacheMatrix
# Function to create a list with functions to:

# 1.set the value of the matrix
# 2.get the value of the matrix
# 3.set the value of the inverse matrix
# 4.get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      Inverse <- NULL
      set <- function(y) {
            x <<- y
            Inverse <<- NULL
      }
      get <- function() x
      setInverse <- function(solve) Inverse <<- solve
      getInverse <- function() Inverse
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)

}


## cacheSolve
# Function to calculate the inverse matrix of matrix 'x' obtained in the makeCacheMatrix function. 
# It first checks if the inverse matrix already exists in the cache. If so, this function skips the 
# calculation to save time. 

cacheSolve <- function(x, ...) {
      Inverse <- x$getInverse()
      if(!is.null(Inverse)) {
            message("getting cached data")
            return(Inverse)
      }
      data <- x$get()
      Inverse <- solve(data, ...)
      x$setInverse(Inverse)
      Inverse
}
