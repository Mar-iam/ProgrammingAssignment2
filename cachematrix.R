## This function creates a special matrix, which is a list containing a function to
##1) set the value of the matrix
##2) get the value of the matrix
##3) set the value of the matrix inverse
##4) get the value of the matrix inverse


makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  
  # set the matrix
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x                                   # return the matrix
  setInverse <- function(inverse) invMatrix <<- inverse # set the inverse of matrix
  getInverse <- function() invMatrix                    # return the inverse
  
  list(set = set, get = get,
       setInverse = setInverse, getInverse = getInverse)
}


##This function calculates the inverse of the special "matrix" created with the 
##above function. However, it first checks to see if the inverse has already 
##been calculated. If so, it gets the inverse from the cache and skips the 
##computation. Otherwise, it calculates the inverse of the data and sets the 
##value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
  invMatrix <- x$getInverse() # check if inverse is cached
  if(!is.null(invMatrix)) {
    message("getting cached inverse")
    return(invMatrix)
  }
  
  # otherwise calculate the inverse
  data <- x$get()
  invMatrix <- solve(data, ...) # calculate the inverse
  x$setInverse(invMatrix) # set the new matrix
  
  invMatrix ## return the inverse
}