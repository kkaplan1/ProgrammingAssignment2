## The functions makeCacheMatrix and cacheSolve allow
## a matrix and its inverse to be set and cached in order to
## reduce time in computing the inverse of the matrix again.

## The makeCacheMatrix function
## Creates a vector which is really a list containing the functions
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
get <- function() x
setinverse <- function(solve) m <<- solve
getinverse <- function() m
list (set = set, get = get, 
      setinverse = setinverse, 
      getinverse = getinverse)
}


## The cacheSolve function
## calculates the inverse of the vector created with the above function
## first checks to see if the inverse has already been calculated
## if it has already been calculated, it gets the inverse from the cache and skips the computation
## otherwise, it calculates the inverse of the matrix
## and sets the value of the inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
