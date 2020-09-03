## Dylan D.
## 9/3/2020
## Coursera R Programming
## Week 3
## Programming Assignment 2


## makeCacheMatrix: This function creates a special "matrix" 
##  object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  ## The inverse of the matrix has yet to be computed
  inverse_value <- NULL
  
  ## Initializing the matrix and inverse value
  set_matrix <- function(y){
    x <<- y
    inverse_value <<- NULL
  }
  
  ## get_matrix: Returns the matrix
  get_matrix <- function() x
  
  ## set_inverse: Caches the inverse value
  set_inverse <- function(inv) inverse_value <<- inv
  
  ## get_inverse: Returns the inverse value
  get_inverse <- function() inverse_value
  
  ## Creating the special "matrix"
  list(set_matrix = set_matrix, get_matrix = get_matrix, 
       set_inverse = set_inverse, 
       get_inverse = get_inverse)
  
}


## cacheSolve: Computes the inverse of the special "matrix" returned 
##  by makeCacheMatrix. If the inverse has already been calculated 
##  (and the matrix has not changed), then the cachesolve retrieves the
##  inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Checking to see if the inverse has been calculated
  m <- x$get_inverse()
  
  ## If there is a value for m != to NULL, the value has been calculated
  if(!is.null(m)){
    message("Getting cached data")
    print("Getting cached data")
    return(m)
  }
  
  ## The inverse has not been calculated so we must calculate it here
  ## Getting the matrix values
  matrix_ <- x$get_matrix()
  
  ## Computing the inverse matrix
  m <- solve(matrix_)
  
  ## Caching the inverse matrix
  x$set_inverse(m)
  
  ## Returning the inverse matrix
  print(m)
  m
  
}