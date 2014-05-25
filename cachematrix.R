##  The 2 functions are designed to return the inverse of a matrix that has
##  already been computed. The functions cache the value and return it when
## the same matric is inputted.

## The 'makeCacheMatric' function takes a matrix as its argument.
## A list is returned.
## The list is to be inputted into the next function to return the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The 'CacheSolve' funciton checks the list returned from previous function.
## If the inverse matrix is found in the list; the inverse is returned.
## If the inverse matric is not found; the inverse matrix is computed and returned

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
