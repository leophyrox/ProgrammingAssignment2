##cache the inverse of a matrix to reduce time to run the code if the inverse is needed multiple times

makeCacheMatrix <- function(x = matrix()) {
  ##matrix is assumed to be invertible
  ##to return a list containing functions to:-
  ##1. Set the Matrix
  ##2. Get the Matrix
  ##3. set the inverse matrix
  ##4. get the inverse
  
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set=set, get=get, setmatrix=setmatrix, getinverse=getinverse)
}


##computes the inverse of the special matrix returned


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  return(m)
  ##if inverse has already been calculated
  ##skip calculation and load it from cache
  if(!is.null(m)){
    message("Getting cached data...")
    return(m)
  }
  ##if not, calculate the inverse
  data <- x$get()
  m = solve(data, ...)
  
  
  x$setmatrix(m)
  return(m)
}
