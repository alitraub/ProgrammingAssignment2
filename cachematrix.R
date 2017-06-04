## makeCacheMatrix is a function that is very similar to the example makeVector
## This function sets the value of a matrix, gets the value of the matrix,
## sets the value of the inverse, and gets the value of the inverse. This data
## is all stored in an 11 x 1 vector.

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
      x <<- y
      im <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) im <<- inverse
    getinverse <- function() im
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve finds the inverse of the special matrix created using makeCacheMatrix
## Before solving the matrix however, it checks to make sure the solution is not already
## available in the cache. We stored the inverse of the matrix as im in makeCacheMatrix.
## In if(is.null(im)) we are checking if im contains a value. If it does, instead of computing
## the inverse of the provided matrix, the function fetches this information from the cache.
## However if im is empty, the function uses the get and setinverse functions to compute the inverse
## for the first time.

cacheSolve <- function(x, ...) {
  im <- x$getinverse()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setinverse(im)
  im
}
