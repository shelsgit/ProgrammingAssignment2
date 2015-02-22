## The makeCacheMatrix function creates a special "matrix" object that caches its inverse

## The cacheSolve function computes the inverse of the special "matrix" returned by the
## makeCacheMatrix function. If the inverse has already been calculated (and the matrix has 
## not changed), then the cacheSolve function retrieves the inverse from the cache


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
