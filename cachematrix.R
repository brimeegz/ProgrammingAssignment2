## makeCacheMatrix and cacheSolve work in conjunction to invert a matrix, but
## only invert the matrix if not previously done.  a stored value is returned
## if previously calculated

## makeCacheMatrix is a function that stores a list of functions
## the functions allow one to get the matrix, set the matrix, get its inverse (potentially null), and set its inverse

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) m <<- inverse
     getinverse <- function() m
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## cacheSolve checks whether matrix inverse m exists, if it exists, m is returned
## if m does not exist, the matrix is retrieved and inverted, and m is returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m <- x$getinverse()
     if(!is.null(m)) {
          message("getting cached data...")
          return(m)
     }
     mat <- x$get()
     m <- solve(mat)
     x$setinverse(m)
     m
}
