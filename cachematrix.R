## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

 m <- NULL   ## Start defining m as NULL, ie initialize
  set <- function(y) {   ##  Take the new matrix and set it to x
    x <<- y
    m <<- NULL           ## Set m as NULL in the global environment
  }
  get <- function() x ## Get the value in the evaluating environment
  setsolve <- function(solve) m <<- solve ## Set the value of solve in parent environment for the variable m
  getsolve <- function() m
  list(set = set, get = get, 
       setsolve = setsolve,
       getsolve = getsolve)    ## Here we list the value of the functions that will be used in cacheSolve
 
}

cacheSolve <- function(x, ...) {

m <- x$getsolve()   ## Retrive and assign the old cache value
  data <- x$get()   ## If x it's not in the cache (wasn't evaluated before) assign x matrix to data
  m <- solve(data, ...) ## Calculate the inverse of the matrix
  x$setsolve(m)    ## Assign the inverse of the matrix to the m environment
  m
  
  
}
