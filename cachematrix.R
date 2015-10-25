## A function to initialize a matrix and store it in a cached variable and define a set of
## sub functions to set the matrix, get a copy of the matrix, and create an iverse of the matrix
## functions do



makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function(){i}
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Function to create an inverse of the passed matrix using the "solve" function. Returns
## the inverse copy already stored if present.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setInverse(i)
    i
  
}
