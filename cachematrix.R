## Assignment 2: Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  set <- function(y=matrix()) 
      {
      x <<- y
      m <<- NULL
      }
  get <- function() x
  cm <- function(solve) m <<- solve
  im <- function() m
  list(set = set, get = get,cm = cm,im = im)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) 
{
  m <- x$im()
  if(!is.null(m)) 
      {
      message("getting cached data")
      return(m)
      }
  data <- x$get()
  m <- solve(data, ...)
  x$cm(m)
  m
}
