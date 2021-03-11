
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #initiate inverse
  inv <- NULL
  #set matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #get matrix
  get <- function() x
  #set inverse
  setinv <- function(solve) inv <<- solve
  #get inverse
  getinv <- function() inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #get cached inverse and return if exists
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inversed matrix")
    return(inv)
  }
  
  #calculate if not exists
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  
  #return value
  inv
}
