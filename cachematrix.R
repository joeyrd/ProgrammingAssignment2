## Together, these functions allow you to calculate the inverse of a matrix and then store it so that it doesn't need to be
## recalculated in future operations. 

## This function takes a matrix and returns a list of functions, each of which is described below.

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
  set<- function(y) { #changes the matrix 
    x <<- y
    inv <<- NULL
  }
  get <- function() x #returns the given matrix
  setinv <- function(i) inv <<- i #set value of inverse matrix to i
  getinv <- function() inv #get value of inverse matrix
  list(set = set, get = get, #return list of all four functions
       setinv = setinv,
       getinv = getinv)
}


## This function takes as input the list created by makeCacheMatrix. If the inverse has been stored, it returns it. If the inverse 
## has not been stored, it calculates it (using solve), then stores it in the list so that it may be accessed later. 

cacheSolve <- function(x, ...) {
       i <- x$getinv() #checks whether stored inverse exists
  if(!is.null(i)) {
    message("getting cached data")
    return(i) #if stored inverse exists, returns it (no calculation necessary)
  }
  data <- x$get() #if there is no stored inverse, this function gets matrix 
  i <- solve(data, ...) #calculates inverse
  x$setinv(i) #stores newly calculated inverse in makeCacheMatrix object
  i #returns calculated inverse 
}
