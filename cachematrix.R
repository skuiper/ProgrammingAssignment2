## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
    }
  get<-function() x
  setmatrix<-function(solve) m <<- solve
  getmatrix<-function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## cacheSolve computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}


## Testing the code:
my_matrix <- makeCacheMatrix(matrix(c(2,1,4,3), 2, 2))
my_matrix$get()         # displays original matrix
my_matrix$getmatrix()   # NULL since no matrix currently exists
cacheSolve(my_matrix)   # calculates the inverse of my_matrix
cacheSolve(my_matrix)   # gets saved inverse instead of recalculating
my_matrix$getmatrix()   # again displays inverse matrix

