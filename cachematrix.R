## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(is.null(inv)) { 
    x$setinverse(solve(x$get()))
    inv <- x$getinverse()
  }
  inv
}

