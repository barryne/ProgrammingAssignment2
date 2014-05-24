## The pair of functions below cache the inverse of a matrix.

## This function ‘makeCacheMatrix’ creates a special "matrix" object 
## that can cache its inverse
## The function will set the value of the matrix, get the value of the matrix,
## set the value of the inverse of the matrix and get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  setinv <- function(solve) invrs <<- solve
  getminv <- function() invrs
        ## Return a list to set and get the value of the matrix and inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function ‘cacheSolve’ computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated, then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invrs <- x$getinv()
         ## If not null return matrix from Cache
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
        ## Else if null assign original matrix
  data <- x$get()
        ## Use solve() to get the inverse
  invrs <- solve(data, ...)
  x$setinv(invrs)
  invrs
}