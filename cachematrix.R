## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
  

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getInverse()
  if(!is.null(m)){
    message("Getting Cached Data")
    return(m)
  }
  data <- x$get()
  m<- solve(data,...)
  m<- m %*% data
  x$setInverse(m)
  return(m)
}
x <- stats::rnorm(16)

 dim(x) <- c(4,4)
x
solve(x) %*% x
temp<-makeCacheMatrix(x)
cacheSolve(temp)
