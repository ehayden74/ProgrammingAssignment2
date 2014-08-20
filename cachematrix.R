
##Function takes a matrix as input and creates a special matrix object that is used for caching matrices
makeCacheMatrix <- function(x = matrix()) {
  
  ##set inverse matrix variable to null
  inv <- NULL
  
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) inv <<- inv
  getmatrix <- function() inv
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

##Function used to cache an inverse matrix
cacheSolve <- function(x, ...) {
  
  ##Checks to see if inverse matrix has been calculated before.
  ##If it has then the cached matrix is returned
  inv <- x$getmatrix()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  
  ##solve function used to calculate inverse matrix for the first time
  inv <- solve(data, ...)
  x$setmatrix(inv)
  inv
}
