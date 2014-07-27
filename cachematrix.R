## Two functions that create a special object that stores a matrix and caches its inverse.
## The first function, makeCacheMatrix, creates a special "matrix" which is a list that:
## - sets the value of the matrix (setmatrix);
## - gets the value of the matrix (getmatrix);
## - sets the value of the inverse (setinverse);
## - gets the value of the inverse (getinverse).

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL 					## Initially assigning 'NULL' to inverse
  setmatrix <- function(y) {			
    x <<- y 					## Setting the matrix 'x'
    inverse <<- NULL
  }
  getmatrix <- function() x 				## Returning matrix 'x'
  setinverse <- function(solve) inverse <<- solve 	## Caches the value of the inverse 
  getinverse <- function() inverse 			## Returning inverse
  list(setmatrix=setmatrix,getmatrix=getmatrix,
       setinverse=setinverse,getinverse=getinverse)
}


## The second function calculates the inverse of the special "matrix" created with the first function.
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the ## cache and skips the computation. 
## If not, it calculates the inverse and sets the value in the cache via the 'setinverse' function.

cacheSolve <- function(x, ...) {			## Returns a matrix that is the inverse of 'x'
  inverse <- x$getinverse()			## Gets the inverse
  if(!is.null(inverse)) {			## Checks for existance of inverse
    message("getting cached data")	## Displays message
    return(inverse)
  }
  data <- x$getmatrix()			## Gets Matrix
  inverse <- solve(data, ...)			## Uses solve() to compute inverse
  x$setinverse(inverse)			## Caches the inverse
  inverse 					## Returns the inverse
}
