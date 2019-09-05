## Together, these functions allow the user to cache the inverse 
## of a matrix (calculated by solve()) so that it can be called 
## later to save computing time.

# makeCacheMatrix creates an environment where "x" is the input
# matrix and "inv" is the inverse of x. The four functions within
# this environment are $set, $get, $setinv, and $getinv, returned 
# as a list. $set and $setinv set the values of x and inv, while 
# $get and $getinv retrieve the cached values of x and inv.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# cacheSolve retrieves the value of inv from makeCacheMatrix, if 
# present, by calling $getinv(). If not, it calculates the inverse 
# of x and sets this as inv within makeCacheMatrix with $setinv().

cacheSolve <- function(cache, ...) {
  inv <- cache$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- cache$get()
  inv <- solve(data, ...)
  cache$setinv(inv)
  inv
}
