## Reduce Processing time for Functions carried out on Complex Matrices
## especially when your matrix changes during the course of program run
## The following 2 functions, caches the 

# makeCacheMatrix creates a special "matrix" object that can cache its inverse.
# 1. Gets and Sets the value of the matrix
# 2. Gets and Sets the Inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# Returns the inverse of the matrix. If Inverse has already been calculated
# returns the last result, else re-computes the inverse and also sets the
# value while returning the result

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("No change in Matrix ..Getting cached data")
    return(inv)
  }
  message("Inverse for New or Changed Matrix ")
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}


# Test Run
x <- rbind(c(4, 7), c(2, 6))
m <- makeCacheMatrix(x)


## No cache in the first run
cacheSolve(m)

## Retrieving from the cache in the second run
cacheSolve(m)

# Matrix changes
x = rbind(c(4,3), c(2,6))
m <- makeCacheMatrix(x)
cacheSolve(m)


# Test for 3 x 3 Matrix
th <- rbind(c(4, 7, 1), c(2, 6, 2), c(1,2,3))
ot <- makeCacheMatrix(th)
cacheSolve(ot)
