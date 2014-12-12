## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function has 4 "methods" that can be called directly or are used with cacheSolve
##  - set(y) : this method will overwrite the current matrix with a new matrix
##  - get() : returns the current matrix
##  - setinverse() : called from cacheSolve, creates the inverse matrix and stores it
##  - getinverse() : returns the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(solve) inv <<- solve
  
  getinverse <- function() inv
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## This function checks to see if the inverse has already been created.  If yes, it
##   retrieves it from the cache.  Otherwise it creates the inverse, stores it, and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}

