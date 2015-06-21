## The 'makeCacheMatrix' function is used to create a special matrix object which can cache 
##the input matrix and the inverse of it

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setMtrx <- function(y) {
    x <<- y
    m <<- NULL
  }
  getMtrx <- function() x
  setInvrs <- function(solve) m <<- solve
  getInvrs <- function() m
  list(set = setMtrx, get = getMtrx,
       setInvrs = setInvrs,
       getInvrs = getInvrs)
}


## The 'cacheSolve' function is used to check whether inverse of a matrix has already 
##been calculated and cached. If yes, it will return the cached inverse matrix and if not,
##it will compute the new inverse matrix and return it.

cacheSolve <- function(x, ...) {
  n <- x$getInvrs()
  if(!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  data <- x$get()
  n <- solve(data, ...)
  x$setInvrs(n)
  n
}