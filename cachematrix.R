## Put comments here that give an overall description of what your
## functions do
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL                   # initialize the inverse value to NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x         # function return matrix
  setsolve <- function(solve) s <<- solve  # function to cache inverse
  getsolve <- function() s                 # function to return cache
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## This function computes the inverse of the special "matrix" returned by 
##   makeCacheMatrix above. If the inverse has already been calculated (and
##	the matrix has not changed), then the cachesolve should retrieve the 
##	inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()   # query matrix x's cache for inverse
  if (!is.null(s)) {
    message("getting cached data")
    return(s)         # return cached value
  }
  data <- x$get()     # get the matrix
  s <- solve(data)    # find the inverse of the matrix using "solve()"
  x$setsolve(s)       # cache the matrix 
  s                   # returning inverse
}
