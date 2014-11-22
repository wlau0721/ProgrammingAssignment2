## The goal of these functions is to make the computation for inverse of a Matrix
## more efficient.  Caching is introduced within the inverse function so that output 
## can be pulled from cache instead of making the inverse computation every time. 

## makeCacheMatrix function will take a matrix parameter and return a function which has four
## other functions inside. The four functions inside are set, get, setsolve and getsolve. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL   ## i is the inverse of the matrix being processed
  set <- function(y) {  ## define the set function to store the matrix to be processed for inverse operation
    x <<- y
    i <<- NULL
  }
  get <- function() x ## define the get function to retrieve the matrix stored within the function
  setsolve <- function(solve) i <<- solve ## define the setsolve function to restore the inverse of matrix
  getsolve <- function() i ## define the getsolve function to retrieve the inverse of matrix
  list(set = set, get = get,  ## return a list of functions consist of set, get, setsolve and getsolve. 
       setsolve = setsolve,
       getsolve = getsolve)
}


## cachesolve function takes a makeCacheMatrix instance which has four functions to store and to retrieve 
## a matrix and its inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getsolve() ## call makeCacheMatrix's getsolve function to get the inverse of the matrix
  if(!is.null(m)) { ## if m is not null
    message("getting cached data") ##print the message 
    return(m) ## return 
  }
  data <- x$get() ## call makeCacheMatrix's get function to get the matrix
  m <- solve(data) %*% data ## calculate the inverse of matrix
  x$setsolve(m) ## call makeCacheMatrix's setsolve function to store the inverse of the matrix
  m
  
}
