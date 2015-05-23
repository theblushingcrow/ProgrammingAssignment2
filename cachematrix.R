## A set of 2 function which, when used together, 
## allow tha caching of the computation of the inverse of an invertible matrix. 
## This is particularly usefull when the inverse needs to be computed repeatedly, 
## and the contents of the matrix are not changing.

##  makeCacheMatrix
##  Given x, an invertible matrix
##  creates a special "matrix", which is really a list containing a function to
##  set the value of the matrix
##  get the value of the matrix
##  set the value of the inverse matrix
##  get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##  makeCacheMatrix
##  Given x, a special "matrix" which is the result of calling makeCacheMatrix
##  returns a matrix that is the inverse of the matrix in 'x'.
##  First it checks to see if the inverse has already been calculated.
##  If it has it returns it from the cache.
##  Otherwise it calculates it and stores the resulting inverse matrix in the cache,
##  for use in future calculations.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}