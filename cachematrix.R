## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## setting the matrix
## getting the matrix
## setting the inv matrix
## getting the inv matrix

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv  <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## usage of these functios
## checking whether the vector mean is already in the memory
## if it is (i.e. does not return NULL), it will get the result from memory
## otherwise, it will actually calculae the inverse of the matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	m <- x$getinv()
	  if(!is.null(m)) {
		message("getting cached data")
		return(m)
	  }
	  data <- x$get()
	  m <- solve(data, ...)
	  x$setinv(m)
	  m
}

## using this to test the functions 
## testmatrix <- matrix(rnorm(16), nrow=4, ncol=4)
## testinvmatrix(testmatrix)

testinvmatrix <- function(input_matrix) {
  
  tempmatrix <- makeCacheMatrix(input_matrix)
  
  print("First Time")
  
  start.time = Sys.time()
  print(cacheSolve(tempmatrix))
  ## cacheSolve(tempmatrix)
  dur = Sys.time() - start.time
  print(dur)
  
  print("Second Time")
  
  start.time = Sys.time()
  print(cacheSolve(tempmatrix))
  ## cacheSolve(tempmatrix)
  dur = Sys.time() - start.time
  print(dur)
}
