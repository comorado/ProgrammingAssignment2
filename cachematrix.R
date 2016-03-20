## Assignment 2: Cache the inverse matrix
## Matrix inversion has complexity O(N^3) where N is the
## size of the matrix. In this assignment we check
## if the inverse matrix was calculated if so the
## the value is loaded into the memory

## Creates "matrix" object which cache its inverse
## Assumptions x is always invertable

makeCacheMatrix <- function(x = matrix()) { 
  ## initialize the value of the matrix inverse to NULL
  matrixinverse <- NULL                     

  set <- function(y) {                      
    x <<- y
    matrixinverse <<- NULL              
  }
  get <- function() x                           
  setinverse <- function(solve) matrixinverse <<- solve 
  getinverse <- function() matrixinverse        
  list(set = set, get = get,                    
       setinverse = setinverse,
       getinverse = getinverse)
}

## Computes the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated
## (and the matrix has not been changed), then the cache solve 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		matrixinverse <- x$getinverse()
		if (!is.null(matrixinverse)) {
			message("getting cached data")
			return(matrixinverse)
		}
		data <- x$get()
		matrixinverse <- solve(data,...)
		x$setinverse(matrixinverse)
		matrixinverse
}
