## package of functions designed to avoid repeated inverse matrix calculation

## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix - wraps matrix into object with ability to cache inverse matrix.
##
## Usage:
##
## makeCacheMatrix(x = matrix()) 
##
## x - is a matrix to be wrapped. If omitted the object returned has empty matrix
##
## return value - object with folowing methods
##   get()          - returns wrapped matrix
##   set(x)         - sets new matrix and resets cached inverse value
##   getinverse()   - returns cached inverse value if available or NULL if not
##   setinverse(x)  - sets new inverse value 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x

  setinverse <- function(v) inv <<- v
  getinverse <- function() inv

  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## cacheSolve - calculates and caches inverse matrix of an argument
##
## Usage:
##
## cacheSolve(x) where x is a wrapped matrix returned from makeCacheMatrix
##
## return value - inverse of the matrix wrapped in x. If x had cached value
## of inverse matrix the cached value is returned. Return value is a raw matrix
## not a result of makeCacheMatrix.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(is.null(inv)) { 
    x$setinverse(solve(x$get()))
    inv <- x$getinverse()
  }
  inv
}

