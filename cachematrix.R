## makeCacheMatrix creates a list containing functions to set, get, set the 
## inverse of, or get the inverse of an inputted matrix.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve takes the inverse of the matrix cached by the makeCacheMatrix 
## function.  If the inverse has already been calculated and is in the cache,
## the inverse will be retrieved from the cache directly.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Retrives inverse from cache:
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Getting cached data...")
    return(inv)
  }
        ## Solves for inverse if not in cache and then returns result:
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  return(inv)
}
