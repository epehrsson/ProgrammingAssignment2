## Functions for special "matrices" to compute and cache the matrix inverse

## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  matrix_inverse <- NULL
  set <- function(y) {
    x <<- y
    matrix_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solved) matrix_inverse <<- solved
  getinverse <- function() matrix_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated, retrieves it from cache
## The matrix supplied must be invertible

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  matrix_inverse <- x$getinverse()
  if(!is.null(matrix_inverse)) {
    message("getting cached data")
    return(matrix_inverse)
  }
  message("calculating matrix inverse")
  data <- x$get()
  matrix_inverse <- solve(data)
  x$setinverse(matrix_inverse)
  matrix_inverse
}