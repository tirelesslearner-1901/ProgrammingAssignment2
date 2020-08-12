## A pair of functions that cache the inverse of a matrix.
## This function creates a special "matrix" object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setmatrix <- function(solveMax) inverse <<- solveMax
  getmatrix <- function() inverse
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getmatrix()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setmatrix(inverse)
  inverse
}
