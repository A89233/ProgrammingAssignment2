# makeCacheMatrix creates a special matrix, which is really a list containing a function to 
# 1. setMatrix: caches the matrix passed as parameter
# 2. getMatrix: gets the matrix
# 3. setInverseMatrix: caches the inverse of the matrix
# 4. getInverseMatrix: gets the inverse of the matrix

makeCacheMatrix <- function(mat = matrix()) {   # matrix as parameter
  invMatrix <- NULL                             # initialize inverse of matrix to null
  setMatrix <- function(matrixtobecached) {     # pass matrix to be cached as parameter
    mat <<- matrixtobecached                  # cache matrix 
    invMatrix <<- NULL                        # clear cache of inverse of the matrix since it is changed matrix
  }
  getMatrix <- function() mat                   # get matrix
  setInverseMatrix <- function(inverseMatrix) 
    invMatrix <<- inverseMatrix  # cache inverse matrix   
  getInverseMatrix <- function() invMatrix         # get inverse matrix
  return(list(setMatrix = setMatrix, getMatrix = getMatrix, # return list for function
              setInverseMatrix = setInverseMatrix, 
              getInverseMatrix = getInverseMatrix))
}


# cacheSove returns inverse matrix from cache if exists
# if inverse matrix doesnot exist in cache, it computes inverse of matrix
# saves the inverse of matrix in cache and returns the inverse of matrix
cacheSolve <- function(mat, ...) {             # matrix as parameter
  
  invMatrix <- mat$getInverseMatrix()          # get inverse of matrix from cache
  if(!is.null(invMatrix)) {                    # check if inverse of matrix exists
    message("getting cached data")
    return(invMatrix)                        # return inverse matrix
  }
  # If inverse matrix does not exist in cache create inverse and set in cache
  matrixData <- mat$getMatrix()                # assign matrix data
  invMatrix <- solve(matrixData, ...)          # inverse matrix data by using function solve()
  mat$setInverseMatrix(invMatrix)              # set inverse matrix data to cache
  return(invMatrix)                            #  return inverse matrix data
}
