## Put comments here that give an overall description of what your
## functions do

## ## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## This function computes the inverse of the special "matrix" which was created above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

##Solution:
my_matrix <- makeCacheMatrix(matrix(c(5,6,8,10), 2, 2))
my_matrix$get()
my_matrix$getinverse()
cacheSolve(my_matrix)
my_matrix$getinverse()


Sample:
  
  > my_matrix$get()
[,1] [,2]
[1,]    5    8
[2,]    6   10

> my_matrix$getinverse()
NULL

> cacheSolve(my_matrix)
[,1] [,2]
[1,]    5 -4.0
[2,]   -3  2.5

> cacheSolve(my_matrix)
getting cached data
[,1] [,2]
[1,]    5 -4.0
[2,]   -3  2.5

> my_matrix$getinverse()
[,1] [,2]
[1,]    5 -4.0
[2,]   -3  2.5
> 