## Programming Assignment 2:
## The following R script contains two functions the support 
## caching potentially time-consuming computations,
## In this case, the functions supports caching the inverse matrix operation 

## makeCacheMatrix function that create and return a vector which is a list 
## of functions for:
## 1.setting the value of the matrix
## 2.getting the value of the matrix
## 3.setting the value of the matrix's inverse
## 4.getting the value of the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setInverseMat <- function(solve) s <<- solve
  getInverseMat <- function() s
  list(set = set, get = get,
       setInverseMat = setInverseMat,
       getInverseMat = getInverseMat)
}

## cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix
## and Return it.
## The function first checks to see if the inverse has already been computed,
## If so, it gets the inversed matrix from the cache and skips the computation,
## Otherwise, it computes the matrix inverse

cacheSolve <- function(x, ...) {
  s <- x$getInverseMat()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setInverseMat(s)
  s
}

