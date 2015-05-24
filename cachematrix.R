## The purpose of the two funtions makeCacheMatrix and cacheSolve is to take an
## invertible matrix and compute its inverse matrix. If the inverse has already
## been computed, it gets restored from the cache.

## makeCacheMatrix stores a matrix ma and returns a list of simple functions for
## getting and setting the matrix and its inverse matrix.

makeCacheMatrix <- function(ma = matrix()) {
    
    inverseMatrix <- NULL
    
    set <- function(y) {
        ma <<- y
        inverseMatrix <<- NULL
    }
    
    get <- function() ma
    
    setInverseMatrix <- function(solve) inverseMatrix <<- solve
    
    getInverseMatrix <- function() inverseMatrix
    
    list(set = set, get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## cacheSolve returns the inverse matrix corresponding to a matrix object
## created via the makeCacheMatrix function. If the inverse matrix has already
## been calculated, cacheSolve returns the inverse from the cache. Else the
## inverse will be calculated and cached with the matrix object.

cacheSolve <- function(ma, ...) {
    
    inverseMatrix <- ma$getInverseMatrix()
    
    if(!is.null(inverseMatrix)) {
        message("getting cached inverse matrix")
        return(inverseMatrix)
    }
    
    matData <- ma$get()
    dimMatData <- dim(matData)[1]
    inverseMatrix <- solve(matData, diag(dimMatData), ...)
    ma$setInverseMatrix(inverseMatrix)
    
    inverseMatrix
}
