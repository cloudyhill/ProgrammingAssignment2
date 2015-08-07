## Matrix inversion is a costly process. We wish to store the inversion of a matrix in the memory so 
## the next time we want to use the inversion, there is no need to compute the inversion again. 

## makeCacheMatrix is a function that creates a special matrix, which stores the inversion of the matrix
## if it has been computed, and ways (functions) to get access to the matrix and its inversion. 

makeCacheMatrix <- function(x = matrix()) {
  inv=NULL
  set=function(y){
    x<<-y
    inv<<-NULL
  }
  get=function() x
  setinv=function(inverse) inv<<-inverse
  getinv=function() inv
  list(set=set, get=get,setinv=setinv,getinv=getinv)
}


## cacheSolve is a function that takes the special matrix created by makeCacheMatrix as argument, then
## either directly return the inversion of the special matrix if it has been computed, or calculate inversion 
## of the special matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv=x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data=x$get()
  inv=solve(data,...)
  x$setinv(inv)
  inv
}
