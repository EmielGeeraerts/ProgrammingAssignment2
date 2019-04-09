# Programming Assignment 2 for R Programming on Coursera

# This script contains two functions necessary to cache the inverse
# of a matrix.

# 1. This function creates a special "matrix" object that can cache its inverse
# The function creates a "matrix" as a list containing functions to 
# a) set the value of the matrix, 
# b) get the value of the matrix,
# c) set the value of the inverse matrix,
# d) get the value of the inverse matrix,
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  #a)
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  #b)
  get <- function() x
  #c)
  setInverse <- function(solve) s <<- solve
  #d)
  getInverse  <- function() s
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# 2. This function calculates the inverse of the special "matrix" created
# by above function
cacheSolve <- function(x, ...) {
  #check if inverse matrix exist in cache
  s <- x$getInverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  #calculate the inverse matrix and save it to cache
  data <- x$get()
  s <- solve(data, ...)
  x$setInverse(s)
  s
  ## Return a matrix that is the inverse of 'x'
}


# Example. Remove "#" to see functions in action. Note the message
# on the second cacheSolve(X) to indicate that the solution was drawn from
# cache

# a <-  matrix(rnorm(1e6),nrow=1e3,ncol=1e3)
# X <- makeCacheMatrix(a)
# cacheSolve(X)
# cacheSolve(X)


