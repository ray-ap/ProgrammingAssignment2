## Matrix inversion is a costly computation. This pair of functions cache the inverse of a matrix.


## creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) minv <<- inverse
  getinv <- function() minv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


#computes the inverse of the special "matrix" returned by makeCacheMatrix above
#If the inverse has already been calculated, then cachesolve retrieves the inverse from cache.
cacheSolve <- function(x, ...) {
  minv <- x$getinv()
  if(!is.null(minv)) {
    message("getting cached inverse matrix")
    return(minv)
  }
  data <- x$get()
  minv <- solve(data, ...)
  x$setinv(minv)
  minv
}