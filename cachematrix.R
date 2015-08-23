## Put comments here that give an overall description of what your
## functions do
## set: sets the matrix for which inverse has to be calculated
## get: retrieves the matrix for which inverse has 
##      to be calculated
## setinv: stores the inverse of the matrix in cache
## getinv: retrieves the inverse of the matrix which is stored in cache

## Write a short comment describing this function
## Returns a list of function closure associated with a matrix 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## This function calculates the inverse of a matrix. If the matrix inverse
## has already been calculated previoulsy it will return the cached data
## otherwise the inverse will be calculated for the first time and then 
## stored in cache for later retrieval.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  inv
  }
