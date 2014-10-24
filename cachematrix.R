## These functions work in conjunction to create and access a special
## matrix that stores it's own inverse in cache for faster computation.


## Creates a special kind of matrix that stores it's inverse in cache.
## Includes functions to get and set matrix as well as get and set inverse.

makeCacheMatrix <- function(x = matrix()) {
  cached_inverse <- NULL
  set <- function(y){
    x <<- y
    cached_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) cached_inverse <<- inverse
  getinverse <- function() cached_inverse
    
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  
}


## Searches cache for existing solution to matrix
## and calculates inverse only if not found in cache.

cacheSolve <- function(x, ...) {
  
  # check if cached matrix exists already
  cached_inverse <- x$getinverse()
  if(!is.null(cached_inverse)){
    message("getting cached inverse")
    return(cached_inverse)        
  }
  
  # calculate inverse of matrix
  my_matrix <- x$get()
  calculated_inverse <- solve(my_matrix, ...)
  
  x$setinverse(calculated_inverse)
  
  # return calculated inverse
  calculated_inverse
    
}
