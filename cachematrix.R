## cacheMatrix.R - R Module providing a simple interface to compute and access the inverse of a matrix

## makeCacheMatrix - Constructor for a 'cached matrix' object
## This function accepts an invertible matrix as input and creates a 'cached matrix' object as output.
## The 'cached matrix'  object holds a list of four function members (set, get, setinv, getinv) whose
## purpose is as follows:
## set: sets a new value for the input matrix and destroys its cached inverse matrix
## get: returns the input matrix
## setinv: sets a new value for the inverse matrix
## getinv: returns the value of the cached inverse matrix
## The 'cached matrix' object also has two data members 'mat' and 'inv' which stand for the matrix passed
## as input and the the inverse of said matrix.
## The 'mat' member is initialized with a copy of the input matrix.
## If no input matrix is passed, an empty one is created by default
## The 'inv' member is initialized to a NULL value

makeCacheMatrix <- function(x = matrix()) {
  inv    <- NULL
  set    <- function(val) { mat <<- val; inv <<- NULL }
  get    <- function()    { mat }
  setinv <- function(val) { inv <<- val }
  getinv <- function()    { inv }
  list(set = set, get = get, setinv = setinv,  getinv = getinv)
}


## cacheSolve - Inverts a matrix, caching the inverted matrix for reuse
## This function accepts a 'cached matrix' pseudo-object created by makeCacheMatrix
## as input and finds out if there a previously cached inverse, if that's the case,
## it returns the cached inverse. Otherwise, it computes the inverse, saves it for
## future use and returns it as output

cacheSolve <- function(x, ...) {
  inv <- cmat$getinv()
  if ( !is.null(inv) ) {
    return(inv)
  }
  mat <- cmat$get()
  inv <- solve(mat, ...)
  cmat$setinv(inv)
  inv
}