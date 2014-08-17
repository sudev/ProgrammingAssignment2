# Function creates a Inverse *cache* matrix object Returns a list of functions
# associated with the object which can be used to get, set and set and cache
# inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  #Set function sets the value to a global enviornment 
  set <- function(y) {
    x <<- y 
    inv <- NULL
  }
  # get - Returns the object
  get <- function() x
  
  # Caches the inverse value 
  setinverse <- function(inverse) inv <<- inverse
  
  # returns the cahed value
  getinverse <- function() inv 
  
  # function returns a list of methods 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# CacheSolve checks for cached value of inverse matrix and returns 
# else creates inverse matrix,caches and returns its inverse
cacheSolve <- function(x, ...) {
  #check if cache for Inverse exists, If yes return then the cached value 
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If caching was not done earlier lets do it now 
  
  # Assign data to variable mat (matrix)
  mat <- x$get()
  # Calculate the inverse value
  inv <- solve(mat) %*% mat
  
  # Set the inverse value to cache memory 
  x$setinverse(inv)
  
  #return the inverse value 
  return(inv)
}