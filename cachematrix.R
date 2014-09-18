## These two functions (makeCacheMatrix and cacheSolve) allow the inverse of a matrix to be cached in order to minimize computation time and resources
## Note: These functions assume the matrix fed into the function is invertible

## makeCacheMatrix is a function that creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The function computes the inverse of the matrix 'x', if it has not alreadu been calculated, 
## or retrieves the inverse from the cache if the inverse has already been calculated

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}