## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x=matrix()){
  inverse <- NULL
  setMatrix <- function(m){
    x <<- m
    inverse <<- NULL
  }
  
  getMatrix <- function(){
    x
  }
  
  setInverse <- function(inver){
    inverse <<- inver
  }
  
  getInverse <- function(){
    inverse
  }
  
  list(setMatrix=setMatrix,getMatrix=getMatrix,setInverse=setInverse,getInverse=getInverse)
  
}


## Write a short comment describing this function


cacheSolve <- function(x, ...){
  matri <- x$getInverse()
  if(!is.null(matri)){
    return(matri)
  }
  data <- x$getMatrix()
  numOfCol <- ncol(data)
  identityMatrix <- diag(numOfCol)
  myInverse <- solve(data,identityMatrix)
  x$setInverse(myInverse)
  myInverse
}
