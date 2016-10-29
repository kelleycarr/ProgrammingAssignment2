## These functions save time by caching results of finding the inverse of a matrix.
## cacheSolve must be run on the result of makeCacheMatrix.

## makeCacheMatrix requires a square matrix as an input. 
## It returns a list of four functions:
##   1. set the matrix (can be used to input a different matrix)
##   2. get the matrix
##   3. set the inverse of the matrix
##   4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL

    if (!is.matrix(y)) {
      message("object must be a matrix")
      x <<- NULL
      return()
    }
  }

  get <- function() x
  
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## cacheSolve takes the result of makeCacheMatrix.
## If the inverse of the matrix has already been calculated, it will retrieve the inverse.
## If the inverse of the matrix has not been calculated, it will calculate the inverse.

cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'
  
  m <- x$getInverse()
  if (!is.null(m) && is.matrix(x$get())) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}
