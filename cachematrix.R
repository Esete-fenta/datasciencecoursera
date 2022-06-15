## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # initializing inverse as NULL 
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x  # function to get matrix X
  setinverse <-function (inverse)inv<<-inverse
  getinverse <- function ()inv
  list(set=set, get=get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse ()
  if(!is.null(inv)){
    message ("getting cached matrix inverse!")
    return (inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
