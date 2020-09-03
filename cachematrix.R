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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$get_inverse()
  if(!is.null(m)){
    message("Getting cached data")
    return(m)
  }
  
  matrix_ <- x$get_matrix()
  m <- solve(matrix_)
  x$set_inverse(m)
  m
  
}

A = matrix(c(1,3,5,7), nrow = 2, ncol = 2, byrow = TRUE)
makeCacheMatrix(A)