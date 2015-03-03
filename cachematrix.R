## The functions in this script generate a "custom" matrix
## and calculate its inverse, storing the inverse in a 
## property of the customized matrix.

## This function generates the "custom" matrix with the
## inverse cached in a property.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates de inverse of the "custom" matrix 'x'
## and, if the customized matrix already has its inverse
## calculated, it retrieves it from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
