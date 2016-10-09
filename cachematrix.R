## The first function is going find the inverse of a matrix and store the inverse matrix.
## The second function is going to either return a stored matrix from the first function or find the inverse matrix.

## This function is going to create a matrix object and cache the inverse of the matrix object.

makeCacheMatrix <- function(x = matrix()) {
    ## First section of code assigns value to the matrix x outside the current environment in the console
  
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
  }

    get <- function() x
 
    ## Using solve function to find inverse for a square matrix
  
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function returns the cache matrix from the makeCacheMatrix function if the inverse has already been found.
## If the inverse has not been found then cacheSolve will find and return the inverse matrix.

cacheSolve <- function(x, ...) {
    ## Looks for a cache Matrix and returns its inverse if found
  
    i <- x$getinverse()
    if(!is.null(i)){
          message("getting cached data")
          return(i)
    }

    ## If a Matrix is not found then calculate the inverse of the Matrix given
    
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
