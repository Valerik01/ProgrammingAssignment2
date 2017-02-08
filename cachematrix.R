# Inverse some matrix, if we'd computed the inverse before - getting this value from cache
# Sample:
# my_matrix <- matrix(...) - simple matrix - it should be square and not singular for correct computation
# sp_matrix <- makeCacheMatrix(my_matrix) - creating special object from matrix for using cache
# inv_matrix <- cacheSolve(sp_matrix) - getting the inverse of matrix my_matrix

# Create a special object, consisting with some functions for caching matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(m_inverse){
    m <<- m_inverse
  }
  
  getinverse <- function() m
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Computes the inverse of special object, created with makeCacheMatrix(A) - where A is a matrix
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data, ...)
  
  x$setinverse(m)
  
  m
}