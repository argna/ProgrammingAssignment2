## Caching the inverse of a matrix
# Matrix inversion is usually a costly computation and there may be some 
# benefit to caching the inverse of a matrix rather than compute it 
# repeatedly.
# Here are a pair of functions that are used to create a special object that 
# stores a matrix and caches its inverse.


## makeCacheMatrix
# This function creates a special "matrix" object that can cache its inverse.
# It's a list containing a function to:
# set the value of the matrix, get he value of the matrix, set the inverse and get the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve
# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache. Otherwise it calculates the inverse and sets the 
# value of the inverse in the cache using the setinverse function
# We assume that the matrix is invertible

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}
