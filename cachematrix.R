## This module implements a cached matrix which optimizes matrix inverse calculation. 
## The function makeCacheMatrix creates a cached matrix from a square non-singular matrix.
## The function cacheSolve calculates and returns the inverse of the cached matrix, or 
## retrieves the inverse from the cache, if it was previously calculated.


makeCacheMatrix <- function(mat = matrix()) {
  # makeCacheMatrix Creates a cached matrix which optimizes repeated calculation 
  #                 of the matrix inverse.
  # Args:
  #   x:    A non singular numeric square matrix.
  #
  # Returns:
  #   A list containing the following four elements named set, get, setinverse, getinvers.
  #   Each element is a function as follows:
  #     set(y) - Saves the matrix y (Non-singular square matrix) in a cache
  #     get() - Returns a previously stored matrix from the cache
  #     setinverse(inverse) - Saves  the inverse of the matrix in the cache
  #     getinverse() returns the inverse of the input matrix stored in the cache.
  #   The above function are internal function, and in most cases should not be called directly by the user.
  inv <- NULL
  set <- function(mat) {
    cached_mat <<- mat
    inv <<- NULL
  }
  get <- function() mat
  setinverse <- function(mat_inverse) inv <<- mat_inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve <- function(cm, ...) {
  # cacheSolve  calculates the inverse of a non-singular square matrix stored in a cached matrix.
  #             If the inversed was already calculated and is stored in the cache, The cached value
  #             is returned
  # Args:
  #   cm - a cached matrix created by the function makeCacheMatrix
  #   ... - further arguments passed to or from other methods
  # Returns:
  #   The inverse of the matrix stored in the cached matrix cm
  inv <- cm$getinverse()
  if(!is.null(inv)) {
    return(inv)
  }
  inv <- solve(cm$get(), ...)
  cm$setinverse(inv)
  inv
}

