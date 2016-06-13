## Put comments here that give an overall description of what your
## functions do

## I adapted the function given on the course websit to fit the 
## requirements of this assignment.

## makeCachematrix takes argument x as matrix and caches NULL. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                 ## create var inv
  set <- function(y) {        ## create cache function
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
      get = get,
      setInverse = setInverse,
      getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {              ## eval if inv contains data 
    message("getting cached data")  ## if TRUE return message and data
    return(inv)
  }
  matrx <- x$get()                     
  inv <- solve(matrx, ...)          
  x$setInverse(inv)                 ## inverse matrix here
  inv
}


