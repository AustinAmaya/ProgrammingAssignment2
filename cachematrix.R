## A pair of functions that create and manipulate an augmented matrix object.
## This augmented matrix caches a copy of its own inverse for convenience.

## This function converts a matrix to an augmented matrix object. The matrix 
## object is actually a list of four functions described below.

makeCacheMatrix <- function(x = matrix()) {
  xInv <- NULL
  
  ## sets the matrix which underlies the CacheMatrix object
  set <- function(y) {
    x <<- y
    xInv <<- NULL
  }
  
  ## returns the underlying matrix
  get <- function() {
    x
  }
  
  ## sets the inverse of the matrix
  setinv <- function(inverse) {
    xInv <<- inverse
  }
  
  ## returns the inverse of the matrix
  getinv <- function() {
    xInv
  }
  
  list(
    set = set, 
    get = get, 
    setinv = setinv, 
    getinv = getinv
  )
}


## Replacement for the solve() function; it acts on CacheMatrix objects. It 
## checks the object for a stored inverse and returns it if found; if a cached
## inverse isn't found then an inverse is computed and stored.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  xInv <- x$getinv()
  
  ## check whether we already have the inverse
  if(!is.null(xInv)) {
    message("getting cached data")
    return(xInv)
  }
  
  ## no luck, we'll have to compute it ourselves
  data <- x$get()
  xInv <- solve(data,...)
  x$setinv(xInv)
  return(xInv)
}
