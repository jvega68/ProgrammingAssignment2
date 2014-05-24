##  The following function creates a list containing a function to
##
## set the value of the list
## get the value of the list
## set the value of the matrix inverse
## get the value of the matrix inverse


## Write a short comment describing this function
# creates a special "matrix" object, which is
# really a list containing a function to
#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse
#4.  get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inv <<- i
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function
# This function computes the inverse of the special 'matrix' returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and 
# the matrix has not changed), then cacheSolve should retrieve the inverse 
# from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get() 
  inv<- solve(data, ...)
  x$setinverse(inv)
  inv
}
