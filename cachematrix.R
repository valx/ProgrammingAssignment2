## This function creates a list of functions related to a matrix in order to cache the result of the inveerse matrix computation
## the input x is an invertible matrix we want to cache the invers of
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # the set function saves the matrix in the x variable in the global environment, and clears m
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # the get functions returns x, where the matrix is normally stored
  get <- function() x
  
  # saves the inverse in the global variable m
  setInverted <- function(inverted) m <<- inverted
  
  # gets the inverse and returns it
  getInverted <- function() m
  
  # returns the list of functions
  list(set = set, get = get,
       setInverted = setInverted,
       getInverted = getInverted)
}


## This function takes makeCacheMatrix result as x input. Then it computes the inverse (if not yet cached) and returns it
cacheSolve <- function(x, ...) {
  m <- x$getInverted()
  
  # check if the inverse was previously computed
  if(!is.null(m)) {
    message("getting cached data")
    # if yes returns the cached inverse
    return(m)
  }
  
  # otherwise: 
  # 1. get the matrix
  data <- x$get()
  # 2. compute its inverse
  m <- solve(data, ...)
  # 3. save the inverse
  x$setInverted(m)
  # 4. return the inverse
  m
}
