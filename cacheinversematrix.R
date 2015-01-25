# This "R" script do the following
# 1.The function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.
# 2.CacheSolve: This function computes the inverse of the special "matrix" returned by function makeCacheMatrix, depicted above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve must retrieves the inverse from the cache.
#
# Function makeCacheMatrix: creates a special "matrix" object.
makeCacheMatrix <- function(x = matrix()) {
  # inv will store the cached inverse matrix
  X_inv <- NULL
  
  # Set the matrix
  set <- function(y) {
    x <<- y
    X_inv <<- NULL
  }
  # Get the matrix
  get <- function() x
  
  # Setter for the inverse
  setinv <- function(inverse) X_inv <<- inverse
  # Getter for the inverse
  getinv <- function() X_inv
  
  # Returning matrix with the newly defined functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# Function cacheSolve: computes the inverse of the special "matrix" returned by function makeCacheMatrix.
# If the inverse has already been calculated, retrieves the inverse from the cache
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  # If the inverse has already been calculated (and the matrix has not changed) return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Calculating the inverse matrix
  data <- x$get()
  inv <- solve(data, ...)
  
  # Caching the inverse matrix
  x$setinv(inv)
  
  # Return the inverse matrix
  inv
}
