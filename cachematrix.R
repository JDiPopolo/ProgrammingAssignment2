## This pair of functions cache the inverse of a matrix
## As matrix inversion is usually a costly computation this may be beneficial to computing it repeatedly

## makeCacheMatrix is a vector that stores a list of functions:
## (1) set - changes the matrix x stored in the main function
## (2) get - returns the matrix x stored in the main function  
## (3) setinv - stores  the value of the inverse in variable inv
## (4) getinv - returns the value of the inverse in variable inv

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                              
  set <- function(y) {                       
    x <<- y                                
    inv <<- NULL                            
  }
  get <- function() {x}                      
  setinv <- function(solve) {inv <<- solve}  
  getinv <- function() {inv}                 
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve works in two steps:
## (1)  verify the value of inv exists and is not #NULL (stored previously with getInv)
## (1a) if yes - return the matrix inverse
## (2)  if no, calculate the inverse of the matrix and store it in the object assigned with makeCacheMatrix

cacheSolve <- function(x, ...) {
  inv <- x$getinv()                
  if(!is.null(inv)) {                 
    message("getting cached data")
    return(inv)                      
  }
  data <- x$get()                    
  inv <- solve(data, ...)             
  x$setinv(inv)                   
  inv        
}
