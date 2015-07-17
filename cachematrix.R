## The two functions below compute the inverse of a matrix and cache results for future use. 
## There are two steps in using these functions. 
## Step 1 - Run makeCacheMatrix with the input matrix. This will output a special object.
## Step 2 - Run cacheSolve with the result from makeCacheMatrix. The result from cacheSolve is the 
##          inverse matrix from the original matrix.

## makeCacheMatrix uses the original matrix to create a special object to help manage the cache of the matrix inverse.
## Input: Original matrix (the matrix that you would like to calculate its inverse).
## Output: A special object which includes functions/data based on the matrix provided.

makeCacheMatrix <- function(x = matrix()) {
  
  matrixInverse <- NULL 
  set <- function(y) { ##allows changing the original matrix
    x <<- y
    matrixInverse <<- NULL
  }
  get <- function() x #returns the original matrix once it's needed to calculate the inverse
  getMatrixInverse <- function() matrixInverse #returns the matrixInverse. This is the cache.
  setMatrixInverse <- function(matrixInverse) matrixInverse <<- matrixInverse #saves the calculated value to the cache
  
  list(set = set, get = get, getMatrixInverse = getMatrixInverse, setMatrixInverse = setMatrixInverse)
}


## Uses the object created by running makeCacheMatrix to either 1) calculate and return the inverse of the original matrix or 
## 2) return the inverse of the original matrix from the cache
## Input: Object created by running makeCacheMatrix
## Output: Inverse of the original matrix (provided to makeCacheMatrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrixInverse <- x$getMatrixInverse()
  
  if (!is.null(matrixInverse)){
    #the inverse is in the cache already, use it
    message("Using the inverse value from the cache")
    return(matrixInverse)
  }
  
  # the inverse is not in the cache, need to compute it
  # and store it in cache
  # get the matrix data
  mxData <- x$get()
  matrixInverse <- solve(mxData,...) #compute it
  x$setMatrixInverse(matrixInverse) #store it in the cache for future use
  matrixInverse 
  
}
