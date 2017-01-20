## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                                  
  
  set <- function(y) {                          
    x <<- y                                   
    inv <<- NULL                        
  }
  get <- function() x                               
  setinv <- function(good) inv <<- good   # inverse set
  getinv <- function() inv             # get inverse     
  list(set = set, get = get,       # list various options               
       getinv = getinv)
  
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")  # will return cached data
    return(inv)  # skip computation
  }
  data <- x$get()
  inv  <- solve(x)  # will solve for matrix inverse.
  
  x$setinv(inv)   #will set matrix inverse.
  inv
}