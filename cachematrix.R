## Put comments here that give an overall description of what your
## functions do

## Mark Kinsky - 27 April 2014

## The following two functions work in tandem to create a special matrix which 
## can be accessed outside its environment via another function.

## Write a short comment describing this function
## The makeCacheMatrix function creates the special matrix.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve() accesses the special matrix via makeCacheMatrix() and returns the special matrix's inverse.  If the inverse is not cached, cacheSolve() will generate it and cache it.
## If the inverse already exists in cache, the inverse is retrieved and the associated computation is bypassed.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
