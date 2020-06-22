## The following functions create a special invertible matrix and
## calculate de inverse of the matix in case it hasn't been calculated
## previously and stores it in cache, otherwise it retrieves it from cache

## Returns a list with the functions to set and get a matrix and it's inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(i) inverse <<- i
  
  getInverse <- function() inverse
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Calculates the inverse of a matrix if it doesn't exits in memory yet,
## otherwise it retrieves it from memoty
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  
  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
  
}
