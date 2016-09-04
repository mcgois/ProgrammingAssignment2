## Put comments here that give an overall description of what your
## functions do

## Creates a data sctructure that caches inverse matrix when inversion 
## is called for the first time (by function cacheSolve).
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


## Resolves the inverse matrix from the structured matrix (makeCacheMatrix)
## The first time the inversion is called, it will store the inverse matrix.
## Following calls will return cached results.
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
