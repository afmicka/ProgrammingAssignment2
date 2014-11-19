## Two functions below are used to create a special object 
## that stores a matrix and cache's its inverse


## The function 'makeCacheMatrix' creates a special "matrix" object 
## that can cache its inverse. It returns a list containing 4 functions - 
## a function to:
##  	1. set the value of the matrix
##  	2. get the value of the matrix
##  	3. set the inverse of the matrix
##  	4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setInverseMatrix <- function(inverseMatrix) m <<- inverseMatrix
            getIverseMatrix <- function() m
            list(set = set, get = get,
                 setInverseMatrix = setInverseMatrix,
                 getInverseMatrix = getInverseMatrix)

}


## The function 'cacheSolve' computes the inverse of the special "matrix" 
## returned by the above function. It first checks to see if the inverse 
## of the matrix has already been computed. If so, it retrieves the inverse 
## from the cache and skips the computation. Otherwise, it computes
## the inverse of the provided matrix and sets it in the cache 
## via the 'setInverseMatrix' function. 

cacheSolve <- function(x, ...) {
            m <- x$getInverseMatrix()
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$setInverseMatrix(m)
            m

        ## Return a matrix that is the inverse of 'x'
}
