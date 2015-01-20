## [These two functions cache the inverse of a matrix. MakeCacheMatrix
##  creates a special matrix object that can cache its inverse. CacheSolve
##  computes the inverse of the matrix. If the inverse has been already 
##  calculated, then te cachesolve should retrieve the inverse form the cache]

makeCacheMatrix <- function(x = matrix()) {
  m <<- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set=set,get=get,getinverse=getinverse,setinverse=setinverse)
  
}


cacheSolve <- function(x, ...) {
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
