## R-Programming - Assignment #2
## Abdul Ghaffar

##  Create Special type of matrix that caches inverseMatrix for later retrieval from matrix passed into it
makeCacheMatrix <- function (x = matrix()) {
  ## Create a special type of matrix with functions to set and get inverse of a given Matrix
  invMatrix <- NULL
  
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setInv <- function(inv) {
    # Setting InverseMatrix here
    invMatrix <<- inv
  }
  
  getInv <- function() {
    # Getting InverseMatrix ...
    invMatrix
  }
  
  list(set=set, get=get, setInv=setInv, getInv=getInv) 
}


# This function first checks for a cached inverseMatrix first; otherwise calcualates/sets/returns a new 
# inverseMatrix which could be retrieved later.
cacheSolve <- function(x, ...) {
  invMatrix <- x$getInv()
  
  if (!is.null(invMatrix)) {
    message(">>> Cached Data found - Returning Cached Data")
    return (invMatrix)
  }
  
  ## If no inverseMatrix returned, calcuate a new one, set it up and return the results.
  message (">>> No Cached Data - Calculating new InverseMatrix for this time")
  
  data <- x$get()
  x$set(data)
  
  invMatrix <- solve(data)
  x$setInv(invMatrix)
  invMatrix
}