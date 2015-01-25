# Cached Matrix inversion (Coursera-Assignment 2)

# makeCacheMatrix function does following using list ::
# 1. set matrix value 
# 2. get matrix value 
# 3. set matrix inverse
# 4. get matrix inverse 

makeCacheMatrix <- function(px = matrix()) {
  #Initialize
  inverse <- NULL
  #set matrix value 
  set <- function(py) {
      px <<- py
      inverse <<- NULL
  }
  #get matrix value 
  get <- function() px
  #set matrix inverse
  setInverse <- function(pinverse) inverse <<- pinverse
  #get matrix inverse
  getInverse <- function() inverse
  #list of 4 functions
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


# cacheSolve function 
# 1. Computes and Returns matrix inverse if it is not already completed 
# 1. Returns matrix inverse from cache if alreay calculated.

cacheSolve <- function(px, ...) {
  inverse <- px$getInverse()
  # Check if already calculated
  if(!is.null(inverse)) {
      message("Getting cached data ...")
      return(inverse)
  }
  # If not already calculated then calculate 
    inverse <- solve(px$get())
    px$setInverse(inverse)
    return(inverse)
}

# Example :: 
## Assuming myMatrix is invertible

# myMatrix = rbind(c(4, 3), c(3, 2))
# myObj = makeCacheMatrix(myMatrix)
# myObj$get()
# cacheSolve(myObj)

## Getting from cache
# cacheSolve(myObj)




