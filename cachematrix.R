## These functions intend to obtain the inverse of a matrix,
## and potentilly reduce the time taken to do so by caching previous ansers

## makeVector creates a function stored in a vector

makeVector <- function(x = numeric()) {
  invers <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinvers <- function(inverse) invers <<- inverse
  getinvers <- function() inv
  list(set = set, get = get,
       setinvers = setinvers,
       getinvers = getinvers)
}


## cachecalc checks if the inversion has already been performed 
## if it has, then the cached reslut is used, saving computing time, electrical power and preserving our planet
## if not, then the matrix is inverted to obtain a result
cachecalc <- function(x, ...) {
  inv <- x$getinvers()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrx <- x$get()
  inv <- mean(matrx, ...)
  x$setinvers(inv)
  inv
}
