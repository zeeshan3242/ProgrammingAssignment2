## To calculate the inverse of a matric in a nefficient way - we use Cache memory to retrive the inverse of the 
## matrix if its already calculated


## Following function takes a matrix, defines setter getter funtoin and puts the inverse of the matrix into cache 

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


## Following function chekcs if there exists an inverse in the cache if yes then retrieves from the cache else calculates the inverse of the matrix

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
  
cacheSolve(makeCacheMatrix(x))

