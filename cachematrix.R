## Put comments here that give an overall description of what your
## functions do

## This function gets the matrix that we aregonna work on, and gets the inverse of that matrix

MakeCacheMatrix <- function(x = matrix())
{
  inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setinverse <- function(inverse){inv <<- inverse}
  getinverse <- function() {inv}
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## With this function we can retrieve the inverse of a matrix if we already solved it with the MakeCacheMAtrix function

CacheSolve<- function(x, ...)
{
  inv <- x$getinverse()
  if(!is.null(inv))
  {
    message("Getting inversed matrix")
    return(inv)
    
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
  
}
