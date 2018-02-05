
# Programming Assignment 2: Lexical Scoping 
# Here are R functions which are able to cache potentially time-consuming and costly computations on matrix inversion. 


##1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

#Creating a matrix object able to cache its inverse

makeCacheMatrix <- function(x = matrix()) {


#Setting the inverse
inverse <- NULL

#Setting the matrix
setMatrix <- function(matrix = matrix()){
  x <<- matrix
}

#Getting the matrix
getMatrix <- function() x

#Setting the inverse of the matrix
setInverse <- function(inverseMatrix = matrix()){ 
  inverse <<- inverseMatrix
} 

#Getting the inverse of the matrix
getInverse <- function() inverse

list(get = getMatrix, set = setMatrix, getI = getInverse, setI = setInverse)
}





##2. cacheSolve: This function computes the inverse of the special "matrix" #returned by makeCacheMatrix above. If the inverse has already been #calculated (and the matrix has not changed), then the cachesolve should #retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  # Return a matrix that is the inverse of 'x' 
  m <- x$getInverse()
  
  # Return the inverse if it's there
  if( !is.null(m) ) {
    message("Getting cached data")
    return(m)
  }
  
  # Get the matrix
  data <- x$get()
  
  # Calculate the inverse
  m <- solve(data)
  
  # Set the inverse
  x$setInverse(m)
  
  # Return the matrix
  m
}    



  
  