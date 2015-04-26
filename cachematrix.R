## Caching the Inverse of Matrix.
## The function makeCacheMatrix creates a matrix and allows the inverse to be cached.
## The function also provides a list containing functions for:

## 1. set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse matrix
## 4. Get the value of the inverse matrix

makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  set <- function(y) {
    m <<- y
    i <<- NULL
  }
  get <- function() m
  setinverse <- function(matrixinv) i <<- matrixinv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns the inverse of the provided "special" matrix
## First it checks if an inverse is already available. If so i returns that cached inverse
## If inverse is not available it calculates the inverse using solve() and caches it before
## before returning the value of the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
  