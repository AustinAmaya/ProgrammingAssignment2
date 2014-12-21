## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  xInv <- NULL
  
  set <- function(y) {
    x <<- y
    xInv <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setinv <- function(inverse) {
    xInv <<- inverse
  }
  
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


## Write a short comment describing this function

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
