## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  
  get <- function() x
  
  setInverseMatrix <- function(iM) inverseMatrix <<- iM
  
  getInverseMatrix <- function() inverseMatrix
  
  list(set = set, get = get, setInverseMatrix = setInverseMatrix, 
       getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverseMatrix <- x$getInverseMatrix()
    if (!is.null(inverseMatrix)) {
      message("getting chached inverse matrix")
      return(inverseMatrix)
    }
    
    m <- x$get()
    inverseMatrix <- solve(m, ...)
    x$setInverseMatrix(inverseMatrix)
    inverseMatrix
}
