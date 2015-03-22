## These functions allow you to quickly get the inverse value of a matrix
## by using caching methods built into the R programming language.


## This function creates a matrix that caches the inverse value by
## using set, get and the inverse function.
makeCacheMatrix <- function(x = matrix()) {
  
  ## start off empty
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  ## define functions
  setInverse  <- function(inverse) m <<- inverse
  getInverse <- function() m
  
  list(set = set, get = get,
       getInverse = getInverse,
       setInverse = setInverse)  
  
}


## This function gets the inverse of a matrix, but it first checks
## if the value has already been calcuated, if so it returns the cached value
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInverse()
  
  if(!is.null(m)) {
    message("getting cached inverse value")
    return(m)
  }
  
  data <- x$get()
  ## calc the inverse value
  m <- solve(data, ...)
  x$setInverse(m)
  m  

}
