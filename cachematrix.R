## Source : cachematrix.R 
## Functions that cache the inverse of a matrix 

######################################################################
#      'makeCacheMatrix' creates a special "matrix" object           #                                                   
#       that can cache its inverse.                                  #    
######################################################################

makeCacheMatrix <- function(x = matrix()) {
  
  m_inverse <- matrix()
  
  ## Setter and Getter functions for matrix 'x'
  set <- function(y) {
    x <<- y
    m_inverse <<- matrix()
  }
  get <- function() x
  
  ## Setter and Getter functions for the inverse of matrix 'x'
  setinverse <- function(mat_inv) m_inverse <<- mat_inv
  getinverse <- function() m_inverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

######################################################################
#      'cacheSolve' computes the inverse of the matrix returned by   #                                                   
#       makeCacheMatrix. It returns cached value if the inverse is   #    
#       already computed.                                            #
######################################################################

cacheSolve <- function(x, ...) {
  
  ## Retrive the inverse matrix of 'x'
  mat_inv <- x$getinverse()
  
  ## Return cached data if the inverse is already computed
  if(!identical(mat_inv, matrix())) {
    message("Returning the inverse from cache")
    return(mat_inv)
  }
  
  ## Compute and store the inverse of matrix 'x'
  data <- x$get()
  mat_inv <- solve(data)
  x$setinverse(mat_inv)
  
  ## Return the computed inverse
  mat_inv  
}
