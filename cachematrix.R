## This function returns an object that stores a matrix and its inverse
## matrix. The inverse matrix is initially null.
## hence this object is a cache for the inverse matrix.
##
## Parameter 'M' is the matrix that should be cached
## Returns a list object that containts accessor functions  for the cached matrix
## ( $get/set_matrix) and its inverse ($get/set_inverse_matrix)
##
makeCacheMatrix <- function(M = matrix()) {
    
    M_1 <- NULL
    setMatrix <- function(N) {
        M <<- N
        M_1 <<- NULL
    }
    getMatrix <- function() M
    setInverseMatrix <- function(inverse) M_1 <<- inverse
    getInverseMatrix <- function() M_1
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## This function returns the inverse matrix of a given cache matrix, caching the result
## for successive calls with the same cached matrix. 
##
## parameter 'cacheMatrix' is the cached matrix to inverse (built from a previous call to makeCache)
## Returns a matrix that is the inverse of cacheMatrix$getMatrix()
cacheSolve <- function(cacheMatrix, ...) {
    # check in cache first
     M_1 <- cacheMatrix$getInverseMatrix()
     if (is.null(M_1)){
         # first time : cache the result
         M_1 <- solve(M, ...) 
         cacheMatrix$setInverseMatrix(M_1)
     }
     else {
         message("cached result")
     }
     M_1
}
