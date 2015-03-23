## Function makeCacheMatrix
##
## Usage:
##
## > m <- makeCacheMatrix(matrix(c(6, 8, 9, 10), nrow=2, ncol=2))
##
## Contains functions to 
##   - set the value of the matrix
##   - getMatrix the value of the matrix
##        > m$getMatrix
##   - setInverse the value of the inverse matrix
##        > m$setInverse(inv)
##   - getInverse the value of the inverse matrix
##        > m$getInverse()

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(inv) im <<- inv
  getInverse <- function() im
  list (set = set, getMatrix = getMatrix,
        setInverse = setInverse,
        getInverse = getInverse)
}


## Function cacheSolve
##
## If the matrix has been already cached it will return the cached matrix
## If not cached it will solve the matrix, cache and return the solve matrix
## Usage:
##
## > cacheSolve(m)
## [,1]  [,2]
## [1,] -0.8333333  0.75
## [2,]  0.6666667 -0.50
##
##

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  im <- x$getInverse()
  if (!is.null(im)) {
    message("Getting cached matrix")
    return (im)
  }
  m <- x$getMatrix()
  im <- solve(m,...)
  x$setInverse(im)
  return(im)
}