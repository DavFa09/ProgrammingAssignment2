## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function( mt = matrix() ) {
  
  ## Introduce inverse property
  i <- NULL
  
  ## Set the matrix with this code
  set <- function( matrix ) {
    mt <<- matrix
    i <<- NULL
  }
  
  ## Get the matrix
  get <- function() {
    ## And Return the matrix
    mt
  }
  
  ## Prepare the inverse function of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    i
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  mt <- x$getInverse()
  
  ## Just return the inverse if its already set
  if( !is.null(mt) ) {
    message("getting cached data")
    return(mt)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  mt <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(mt)
  
  ## Return the matrix
  mt
}
