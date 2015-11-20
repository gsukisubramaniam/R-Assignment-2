## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y=matrix()) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  cm <- function(solve) m <<- solve
  im <- function() m
  list(set = set, get = get,
       cm = cm,
       im = im)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$im()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$cm(m)
  m
}
