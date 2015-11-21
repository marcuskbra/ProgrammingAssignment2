## Create a cache matrix object that can be used to solve the inverse
## of the matrix, but only calculates the inverse once.
##
## Usage:
##  M <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
##  cacheMatrix <- makeCacheMatrix(M)
##  cacheSolve(cacheMatrix)
##
##  cacheMatrix$set(M)      # Change the matrix being cached.
##  M <- cacheMatrix$get()  # Returns the matrix being cached.
##
##  cacheMatrix$setInverse(solve(data, ...)) # Private function containing cached inverse of x
##  cacheMatrix$getInverse()                 # Private function used to get the cached inverse of x

## Create a cacheMatrix object for an invertable matrix.
makeCacheMatrix <- function(x = matrix()) {
  ## setting the cachedInverse to NULL as a placeholder for a future value
  cachedInverse <- NULL
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cachedInverse <<- inverse
  getInverse <- function() cachedInverse
  ## creates a special "vector", which is really a list containing a functions to
  ## set / get a matrix, and to set / get the inverse of matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The following function calculates the inverse of an cacheMatrix object
## created with the above function. However, it first checks to see if 
## the inverse has already been calculated. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the 
## inverse of the data and sets the value of the inverse in the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invFunc <- x$getInverse()
  ## check if exists previous cached inverse of 'x'
  if(!is.null(invFunc)) {
    ## Return the inverse from cache
    return(invFunc)
  }
  data <- x$get()
  ## Inverse the data
  invFunc <- solve(data, ...)
  ## Set the inverse to cache
  x$setInverse(invFunc)
  invFunc
}
