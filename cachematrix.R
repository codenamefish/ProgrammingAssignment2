## Objective: caching and retrieving the inverse of a matrix

## makeCacheMatrix accepts a matrix as an argument and returns a list of 4 functions
## which set the value of the matrix, retrieve the matrix, set the inverse matrix,
## and get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function (y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve accepts the object produced by makeCacheMatrix to retrieve the inverse matrix
## (if already calculated) or else calculate and return this inverse for the first time

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if (!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
      
}
