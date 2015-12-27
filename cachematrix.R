## It is a benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  
  

    cachedInverse <- NULL
    set <- function(y) {
      x <<- y
      cachedInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) cachedInverse <<- inverse
    getInverse <- function() cachedInverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}



## The below  function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been computed (and the 
## matrix has not changed), then inverse from the cache would be retrieved.


cacheSolve <- function(x, ...) {
  
 
    ## Return a matrix that is the inverse of 'x'
    invFunc <- x$getInverse()
    if(!is.null(invFunc)) {
      message("getting cached data")
      return(invFunc)
    }
    data <- x$get()
    invFunc <- solve(data, ...)
    x$setInverse(invFunc)
    invFunc
  
}
