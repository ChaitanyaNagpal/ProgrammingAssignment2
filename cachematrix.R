## The functions try to make the expensive process of matrix inverse
## cheaper by cashing results of the pre-calculated matrix inverses

## saving matrix and inverse pairs to cache and running cache set and get 
## operations

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inve) inv <<- inve
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## computing and checking the presence of x in the cache before making 
## new call to x to solve for inverse

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.na(inv)){
    message("getting cache data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(x, ...)
  x$setinv(inv)
  inv
}
