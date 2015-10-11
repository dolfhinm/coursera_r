
# function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  matrixinverse <- NULL
  
  # setting a new matrix
  set <- function(y) {
    x <<- y
    matrixinverse <<- NULL
  }
  
  #getting a matrix
  get <- function() x
  
  #setting with inverse of matrix
  setinverse <- function(solve) matrixinverse <<- solve
  
  # getting inverse of matrix 
  getinverse <- function() matrixinverse
  
  #return a list of functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# function computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve<-function(x,...){
  
  # assigning cached value
  matrixinverse <- x$getinverse()
  
  #checking on NULL
  if(!is.null(matrixinverse)) {
    message("getting cached data")
    return(matrixinverse)
  }
  
  # getting matrix and calculating inverse
  m<-x$get()
  matrixinverse<-solve(m)
  
  # set value on inverse matrix in cache
  x$setinverse(matrixinverse)
  
  # return a matrix, which is the inverse
  matrixinverse
}

#solution
a<-makeCacheMatrix(matrix(1:4,nrow=2,ncol=2))
a$get()
cacheSolve(a)
a$getinverse()
a$set(matrix(c(3,5,7,6),nrow=2,ncol=2))
cacheSolve(a)
a$get()
