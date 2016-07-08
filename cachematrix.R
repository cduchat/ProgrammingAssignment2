## MakeCacheMatrix and cacheSolve speedup matrix inversion calculation by
## respectively caching the inverse of a matrix, and recovering the cached
## inverse if available (and solving it if not available)

## MakeCacheMatrix creates a matrix that caches its own inverse, it contains
## the subfunctions 'set' to set the matrix, 'get' to get the matrix,
## 'setinv' to set the inverse, and 'getinv' to get the inverse

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


## cacheSolve returns the inverse of a matrix, if the matrix inverse has been
## cached, then the function returns the cached data and skips the computation

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if (!is.null(inv)) {
            message("Getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
