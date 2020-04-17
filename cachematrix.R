## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix will set the matrix and cacheSolve will calculate inverse
## Write a short comment describing this function
## This function create object that can inverse its cache
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                             
  set <- function(y) {                     
    x <<- y                            
    inv <<- NULL                        
  }
  get <- function() x                     
  
  setinverse <- function(inverse) inv <<- inverse  
  getinverse <- function() inv                     
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)   
  
}


## Write a short comment describing this function
## If inverse has been calculated before itself then this function will bring it from cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
