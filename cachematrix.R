
## This function creates a special "matrix" object that can cache its inverse. 
## Write a short comment describing this function

makeCacheMatrix <- function(mtx = matrix()) {
  inverse <- NULL
 ## set function changes the vectore stored in the main function
    set <- function(x) {
    mtx <<- x;
    ## restore the null value to inverse
    inverse <<- NULL;
  }
  get <- function() return(mtx);
  ##stores the value only, not the inverse
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
  ##used to put it all together, the object will have all the functions
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## Write a short comment describing this function

cacheSolve <- function(mtx, ...) {
## this is to verify the value of inverse, if it exists and not NULL  
inverse <- mtx$getinv()
  if(!is.null(inverse)) {
    message("R is for relax.. Getting cached data...")
    return(inverse)
  }
  ## if inverse is not NULL, it continues to this.
  data <- mtx$get()
  invserse <- solve(data, ...)
  ## data stored here from matrix.
  mtx$setinv(inverse)
  return(inverse)
}
