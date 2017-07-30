#Name: Michelle Apunte

#Below are a pair of functions that are used to create a special object that can store a matrix as well as caches its inverse.

# getMatrix to get matrix, setMatrix to set matrix
# getInvMatrix to get inverse of matrix, setInvMatrix to set inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  getMatrix <- function(){
    x
  }
  setMatrix <- function(y){
    x <<- y
    invMatrix <<- NULL
  }
  
  getInvMatrix <- function(){
    invMatrix
  }
  
  setInvMatrix <- function(invMatrixParam){
    invMatrix <<- invMatrixParam
  }
  
  list(getMatrix = getMatrix,setMatrix = setMatrix,
       getInvMatrix = getInvMatrix,setInvMatrix = setInvMatrix)
}

# Try to get inverse matrix if not null, If NULL checks matrix is invertable using det function
# If matrix is not invertable returns NULL, If matrix is invertable sets inverse matrix and return inverse matrix

cacheSolve <- function(x, ...) {
  invMatrix <- x$getInvMatrix()
  
  if(!is.null(invMatrix)){
    message("Returning cached matrix")
    return(invMatrix)
  }
  
  data <- x$getMatrix()
  
  if(det(data) == 0){
    message("Non invertable matrix returning NULL")
    return(NULL)
  }
  
  invMatrix <- solve(data)
  x$setInvMatrix(invMatrix)
  invMatrix
}

Example

x<-matrix(c(1.45,2.65,3.56,5.46),2,2)
x
######[,1] [,2]
#[1,] 1.45 3.56
#[2,] 2.65 5.46

cache<-makeCacheMatrix(x)

inversa<-cacheSolve(cache)
inversa
###########[,1]       [,2]
#[1,] -3.599209  2.3467370
#[2,]  1.746869 -0.9558339