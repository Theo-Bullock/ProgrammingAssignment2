## solve(M)returns the inverse of an invertable matrix M. This script
## contains two functions so that the function composition 
## cacheSolve(makeCacheMatrix(M)) also returns the inverse of M, but is
## more efficient for repeated calculation.


## This function takes an invertable matrix as its argument and returns 
## a list of four functions, one of which stores the inverse of the
## argument matrix.

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  SetInverse <- function(solve) Inv <<- solve
  GetInverse <- function() Inv
  list(set = set, get = get,
       SetInverse = SetInverse,
       GetInverse = GetInverse)

}


## This function checks if the inverse is already stored in the cache and,
## if not, calculates the inverse and returns it.

cacheSolve <- function(x, ...) {
  Inv <- x$GetInverse()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data, ...)
  x$SetInverse(Inv)
  Inv
        ## Return a matrix that is the inverse of 'x'
}
