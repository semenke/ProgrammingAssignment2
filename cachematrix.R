## Put comments here that give an overall description of what your
## functions do
## These functions will store the inverse of a matrix or compute the inverse if it is not stored

## Write a short comment describing this function
## This is the function that saves the inverse of a matrix
## This function will not calculate the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This function first checks if the inverse of the matrix is stored
## If the inverse is stored, it returns the inverse
## If the inverse is not stored, it calculates the inverse and returns it

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
