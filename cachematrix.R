## Assignment: Caching the Inverse of a Matrix

## For this assignment I will solve for the inverse of a matrix.  
## The assignment has two functions.  The first function will store a matrix, 
## and the second function will compute the inverse of the matrix.

## In the first function "makeCacheMatrix" a matrix is created by setting the values
## of the matrix, get the values of the matrix, set the values of the inverse,
## and get the values of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse = function(inverse) inv <<- inverse
  getinverse = function() inv
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
}


## The next function solves for the inverse of the matrix created
## above.  It checks if the inverse was already solved, and if so
## it gets the inverse from the cache and skips calculations.  If
## the inverse was not solved, it will calculate the inverse of 
## the matrix, set the values of the inverse, and return the 
## results.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting the cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
