## Put comments here that give an overall description of what your


makeCacheMatrix <- function(x=matrix()) {

# Create an object with the property of saving the matrix and it's inverse
  
  inv <- NULL                 # Create inverse as empty
  
  set <- function(y) {        # define function that set matrix in object x
    x <<- y                   # set matrix
    inv <<- NULL              # reset inverse matrix to empty.
  }
  get     <- function() x     # define function that return original matrix x
  setinv  <- function(solve) inv <<- solve  # define function that store inverse in variable 'ínv'
  getinv  <- function() inv   # define function that return inverse from cache 
  
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of matrix 'x'
  
  inv <- x$getinv()         # if inv is empty, the inverse is calculated
  if(!is.null(inv)) {
    message("getting cached data") 
    return(inv)             # get the inverse from the matrix cache.
  }
  data <- x$get()           # asign to 'data' the original matrix
  inv <- solve(data, ...)   # calculated inverse.
  x$setinv(inv)             # add calculated inverse to cache of object x.
  inv
}