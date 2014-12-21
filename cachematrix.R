## The following two functions first calculate the inverse of a matrix
## then save it to the cache. When the same computation must in future
## be made, it can be skipped and the stored value used instead.

## Function 1...
## The first function, makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Create a matrix object x and sub-functions for 
  ## storing and retrieiving its inverse
  
  ## initialize cache m
  m <- NULL
  set <- function(y) {
    x <<- y ## assign the input matrix y to the variable x in parent environment
    m <<- NULL ## re-initialize cache m in parent environment
  }
  get <- function() x ## return the matrix x
  setinverse <- function(inverse) m <<- inverse ## set cache m equal to inverse
  ## of matrix x
  getinverse <- function() m ## return the cached inverse of x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Function 2...
## The following function calculates the inverse of the special "matrix" created
## with the above function. However, it first checks to see if the inverse
## has already been caclulated. If so, it 'get's the inverse from the cache
## and skips the computation. Otherwise, it calculates the matrix inverse
## and sets the value of the inverse in the cache via the 'setinverse' function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  
  ## if cache m contains the inverse of matrix x, return it
  if(!is.null(m)) { 
    message("getting cached data")
    return(m)
  }
  ## else calculate the inverse of x, store it in m, and print m
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}