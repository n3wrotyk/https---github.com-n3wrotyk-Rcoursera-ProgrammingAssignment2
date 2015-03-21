## Put comments here that give an overall description of what your
## functions do

## creates a makeCacheMatrix object that contains two elements 
## d_Matrix square matrix
## d_inverseMatrix that's the inverse(d_Matrix)
makeCacheMatrix <- function( a_x = matrix()) {
  ## define members
  d_inverseOfMatrix <- NULL
  d_Matrix <- a_x
  
  ## set new matrix to makecacheMatrix
  setValue <- function( a_value = matrix() ){
      d_Matrix <<- a_value
      ##reset inverse value
      d_inverseOfMatrix <<- NULL
  }
  
  ## get matrix from makeCacheMatrix
  getValue <- function(){
    d_Matrix
  }
  
  ## set inverse matrix of x
  setInverse <- function(a_inverseValue) {
    d_inverseOfMatrix <<- a_inverseValue
  }
  
  ## get inverse matrix of x
  getInverse <- function(){
    d_inverseOfMatrix  
  }
  
  ##return operation list of make cacheMatrix 
  list(set = setValue,
        get = getValue,
        setInverse = setInverse, 
        getInverse = getInverse)
}


## Return a matrix that is the inverse of a_x
cacheSolve <- function( a_x ) {
        
  inverseOfMatrix <- a_x$getInverse()
  ##evaluate if inverse of Matrix was previously calculated
  if( !is.null(inverseOfMatrix)){
      message("getting cached data :D")
      return (inverseOfMatrix)
  }
  
  message("It's the first time! Solve result for the future")
  ## if not, we have to solve it and stored for future request
  matrix <- a_x$get()
  inverseOfMatrix<-solve(matrix)
  ##store inverse value
  a_x$setInverse(inverseOfMatrix)
  ## return inverse of matrix
  inverseOfMatrix  
}

## run this function to test cacheSolve and makeCacheMatrix
test <- function() {
  message("Starting Test...")
  A <- matrix(c(1, 0, 0, 1), nrow=2, ncol=2)
  B <- matrix(c(1, 0, 1, 1), nrow=2, ncol=2)
  C <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow=3, ncol=3)
  message("Creating test matrix")
  message("matrix A:")
  print(A)
  message("matrix B:")
  print(B)
  message("matrix c:")
  print(C)
  
  cacheA <- makeCacheMatrix(A)
  cacheB <- makeCacheMatrix(B)
  cacheC <- makeCacheMatrix(C)
  
  message("Solve matrix A")
  cacheSolve(cacheA)
  message("Solve matrix B")
  cacheSolve(cacheB)
  message("Solve matrix A again")
  cacheSolve(cacheA) 
  message("Solve matrix C")
  cacheSolve(cacheC)
  message("Solve matrix B again")
  cacheSolve(cacheB) 
  message("success!")
}
