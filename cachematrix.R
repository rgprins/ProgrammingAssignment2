## These two functions are used to cache computationally intensive
## creation of the inverse of matrices. There are two functions involved:
## a) makeCacheMatrix, which creates a special list/matrix and 
## b) cacheSolve, which calculates the inverse of the matrix, 
## if it is not stored in the cache. Otherwise it returns the cached matrix

## The makeCacheMatrix creates a special list containing four functions:
## 1) setting the values of the matrix
## 2) getting the values of the matrix
## 3) setting the value of the inversed matrix
## 4) getting the value of the inversed matrix
  
  makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## function to set the values of the matrix (function 1)
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    ## function to get the values of the matrix (function 2)
    get <- function() x
    
    ## function to set the values of the inversed matrix (function 3)
    setsolve <- function(solve) m <<- solve
    ## function to get the values of the inversed matrix (function 4)
    getsolve <- function() m
    ## store all the functions in a list
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
  }


## cacheSolve inverses a matrix if it is not cached. It then caches it. If the
## inversed matrix is already cached, it returns the cached matrix

  cacheSolve <- function(x) {
    
    ## check whether inversed matrix is already in cache    
    m <- x$getsolve()
    ## if in cache, return cached data
    if(!is.null(m)) {
      message("Getting data from cache")
      return(m)
    }
    
    ## if not in cache, get data 
    data <- x$get()
    ## and inverse matrix
    m <- solve(data)
    ## store inversed matrix in cache
    x$setsolve(m)
    m
    
  }
