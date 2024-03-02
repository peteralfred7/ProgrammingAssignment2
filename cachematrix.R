## Programming Assignment # 2
## by Peter Alfred Martinez

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## initialize to NULL
  inv <- NULL
  
  ## set what would happen to the initial cached matrix created and the changes made to the cached matrix.
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() {
    x
  }
  
  
  ## get the inverse using solve function
  setinverse <- function(solve) {
    inv <<- solve
  }
  
  ## get the inverse matrix
  getinverse <- function() {
    inv
  }
  
  ## passes the value of the function makeCacheMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  # if the inverse exists, get it.
  if( !is.null(inv) ) {
    message("getting cached data")
    return(inv)
  }
  
  #if the inverse does no exist, compute for the inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}


#Testing the code
# generating a random 3x3 matrix
ms <-matrix(rpois(9,3),nrow=3)
ms

#Actual testing 
m1 <- makeCacheMatrix(ms)
m1
cacheSolve(m1)
