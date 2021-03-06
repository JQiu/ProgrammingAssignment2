## I've used Google's R style guide as a style reference:
## https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml

makeCacheMatrix <- function(x = matrix()) {
  ## Creates a special vector - list containing functions to:
  ##    1. set the value of the matrix
  ##    2. get the value of the matrix
  ##    3. set the value of the inverse
  ##    4. get the value of the matrix
  ##
  ## Args:
  ##    x: an invertible matrix
  ##
  ## Returns:
  ##    list containing the set / get functions

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Calculate the inverse of the matrix stored from calling makeCacheMatrix
  ##
  ## Args:
  ##    x: list of set / get functions from makeCacheMatrix
  ##
  ## Returns:
  ##    inverse of matrix stored in x

  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
