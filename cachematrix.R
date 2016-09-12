## makeCacheMatrix and its client function cacheSolve work together
## to decrease the computational cost of repeatedly inverting a 
## a given matrix.

## makeCacheMatrix implements object caching to save the cost of 
## (re)computing the inverse of a matrix if its value has been 
## computed during a previous call to makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}



##  Leverages makeCacheMatrix to avoid reevaluating the inverse
## of matrix x.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
  