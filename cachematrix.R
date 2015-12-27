## I find these 2 functions not so smart, since you have use all of these two functions 
## togethet when you want to calculate an inverse of your matrix, for example: if your
## matrix is m1, then you should use: v1 = makeCacheMatrix(m1) cacheSolve(v1)
## In this way, we have to create related makeCacheMatrix function for each matrix we want
## to use, and the edge is that we don't need to calculate twice, we can just find the 
## inverse of our matrix from the makeCacheMatrix funciton.



## This function is to create a place for each matrix we use, it doesn't do any calculations
## itself, just to save the inverse matrix once you calculate it.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) m <<- inverse
  
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## This function is to find the inverse first, if the makeCacheMatrix doesn't save its inversion,
## it will calculate it, and then save it in the makeCacheMatrix position.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
