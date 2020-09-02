## Corsera Assigment 2, Week 3
## Creates a matrix and then caches it to inverse the object

## Function below has four functions that can be called with $
## $set = take what is passed to it, then set it to parent x
## $get = prints the matrix x
## $setInverse = take the matrix and then set it to the function inv 
## $getInverse = return the function inv
## list will run when the makeCacheMatrix is ran

makeCacheMatrix <- function(x = matrix()) {
        makeCacheMatrix_inv <- NULL
        set <- function(y) {
                x <<- y
                makeCacheMatrix_inv <<- NULL
        }
        get <- function() {
                x
        }
        setInverse <- function(my_inverse) {
                makeCacheMatrix_inv <<- my_inverse
        }
        getInverse <- function() {
                makeCacheMatrix_inv
        }
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the makeCacheMatrix
## Input: x = the matrix and any additional paramaters
## call getInverse from above to get the inversed matrix and store it locally
## check if the inversed matrix is not NULL and print message
## get the matrix from the original makeCacheMatrix call
## using builtin solve, process the matrix and update local value
## send the updated value back to the makeCacheMatrix via the setInverse call
## print out the updated inversed matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cacheSolve_inv <- x$getInverse()
        if (!is.null(cacheSolve_inv)) {
                message("getting cached data")
                return(cacheSolve_inv)
        }
        cacheSolve_data <- x$get()
        cacheSolve_inv <- solve(cacheSolve_data, ...)
        x$setInverse(cacheSolve_inv)
        cacheSolve_inv
}
