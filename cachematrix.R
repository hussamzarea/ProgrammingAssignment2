################# Week 3 Assignment 2  #####################
## function 1
## function makeCacheMatrix caches the inverse of a matrix.
## It creates an matrix object that caches its inverse
makeCacheMatrix <- function(mat = matrix()) {
  invMat <- NULL
  set <- function(y) {
    mat <<- y
    invMat <<- NULL
  }
  get <- function() mat
  setInverse <- function(inverse) invMat <<- inverse
  getInverse <- function() invMat
  list(set = set,
       getinvMat = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## testing the function
makeCacheMatrix(matrix(1:20, 4, 4))

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++==
## function 2 

##function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(xInv, ...) {

  inv <- xInv$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- xInv$get()
  inv <- solve(mat, ...)
  xInv$setInverse(inv)
  xInv
}
