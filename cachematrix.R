## Landon Mortensen
## This code solves the inverse of a matrix and caches the value for future reference

## makeCacheMatrix function sets the matrix 'x' and the solution 'm' and
## creates mutators and accessers to be accessed in the second function
## The key is "set" which initializes both the solution 'm' and returns 'X'

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}

## This function uses the four functions above. If a solution is cached, it's returned.
## If no cache, then it solves and stores.


cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}