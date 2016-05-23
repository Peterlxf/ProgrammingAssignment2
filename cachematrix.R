## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {  # set the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x  # get the value of the matrix
  setinverse <- function(solve) m <<- solve  #set the inverse of the matrix
  getinverse <- function() m  # get the inverse of the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  ## a special matrix has been created
}


## Write a short comment describing this function

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
          m <- x$getinverse()
  if(!is.null(m)) {  # to see whether  the inverse has been calculated above
    message("getting cached data")
    return(m)
  }
  data <- x$get()  # get the value of the matrix
  m <- solve(data)  # calculate the inverse of matrix
  x$setinverse(m)  # set the inverse in the cache via setinverse function
  m  # Return a matrix that is the inverse of 'x'
}

#example
##x <- rbind(c(1,2,3),c(2,3,1),c(4,2,1))
##m <- makeCacheMatrix(x)
##cacheSolve(m)
