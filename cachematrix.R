## Put comments here that give an overall description of what your
## functions do

## This function creates a list objects that contains the matrix.
## It has 4 functions that can be called to create the internal matrix and
## then pass the matrix in an arugment. This then returns the internal matrix, that
## sets the inverse martix to pass a matrix. If an inverse matrix already exists, 
## it will retireve the existing inverse matrix.

makeCacheMatrix <-function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(p) m <<- p
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## This function when called accepts the special 'matrix'. The special matrix is 
## a list that returns a martix in the argument. When called it will attempt to 
## return an inverse matrix. If the inverse matrix does not exist, it create an 
## inverse matrix using the solve argument. The solve argument will pass back the 
## special matrix from where it is stored and then creates the inverse matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}