## Create the fist function that will cache the matrix inverse
makeCacheMatrix <- function(x = matrix()){
  ## i represents the inverse value
  i <- NULL
  ## sets the value of the matrix
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  ## get retrieves the value of the matrix
  get <- function() x
  ## seti sets the value of the inverse
  seti <- function(inv) i <<- inv
  ## geti retrieves the value of the inverse
  geti <- function() i
  list(set = set, get = get, seti = seti, geti = geti)
}

## computes the inverse of the matrix created in first function
cacheSolve <- function(x2, ...){
  i <- x2$geti()
  ## checks to see if inverse has already been computed
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  ## if the inverse hasn't been computed it now is computed 
  data <- x2$get()
  i <- solve(data, ...)
  ## the value of the inverse is set uning the seti function
  x2$seti(i)
  ## returns the inverse
  i
}
