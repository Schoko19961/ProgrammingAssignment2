## Two helper functions which improve performance when dealing 
## repeatedly with the same inverse of a matrix

## Creates an improved Matrix object which allowes caching of a matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(m) {
    x <<- m
    i <<- NULL
  }
  
  get <- function()
    x
  
  setInverse <- function(inverse)
    i <<- inverse
  
  getInverse <- function()
    i
  
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


## helper function which optimizes the computation of the inverse of a matrix
## by caching the result once it is calculated

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("Getting cached inverse")
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix)
  x$setInverse(inverse)
  inverse
}
