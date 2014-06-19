## This couple of R functions are able to cache potentially time-consuming computations. 
## These two functions will take advantage of the scoping rules of the R language and 
## how they can be manipulated to preserve state inside of an R object.
##
## The first function, makeCacheMatrix, creates a special "matrix",
## which is really a list containing functions to:
##
##    set the value of the matrix
##    get the value of the matrix
##    set the value of the inverse matrix
##    get the value of the inverse matrix
##
## The second function calculates the inverse matrix of the special "matrix" created with the above function.
## However, it first checks to see if the inverse matrix has already been calculated.
## If so, it gets the inverse matrix from the cache and skips the computation.
## Otherwise, it calculates the inverse matrix of the data and sets the value of the inverse matrix
## in the cache via the setinverse() function.


## The following function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL # this assginmetn makes sure that every time the matrix is updated, its inverse is reset.
  }
  get <- function() x
  setinverse <- function(matrixInv) inv <<- matrixInv
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated, then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
