## Two functions that cache the inverse of a matrix

## The following function creates a special “matrix” that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  w <- NULL
  set <- function(y){
    x <<- y
    w <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) w <<- inverse
  get_inverse <- function() w
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## The following function computes the inverse of the special "matrix" returned 
# by makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  w <- x$get_inverse()
  if(!is.null(w)){
    message("getting cached data")
    return(w)
  }
  data <- x$get()
  w <- solve(data, ...)
  x$set_inverse(w)
  w      
}
