## I want a function to cache a matrix and its inverse, 
## with methods to get and set the inverse.
## I want another function to determine the inverse of the matrix.
## If the inverse is already cached, it shall used the cached value and return it. 
## If not, it shall calculate it, store it in the cache and return it.

## create a matrix object with functions to get and set its inverse

makeCacheMatrix <- function(x = matrix()) {
        invers <- NULL
        # no set method, because the original matrix object shall not be changed
        getMatrix <- function(){x}
        setInverse <-function(newValue){invers <<- newValue}
        getInverse <- function(){invers}   
        list(getMatrix=getMatrix, 
             setInverse=setInverse,
             getInverse=getInverse)        
}

## Return a matrix that is the inverse of 'x'
## Look the value up in the environment of x and return it if it is there.
## If not, calculate it, set it in the environment of x and return it.

cacheSolve <- function(x, ...) {
        ## does the inverse already exist?        
        invers <- x$getInverse()
        if(!is.null(invers)) {
                message("getting cached data")
                return(invers)
        }  
        ## calculate the inverse
        mymatrix <-x$getMatrix()
        invers <- solve(mymatrix,...)
        ## store it
        x$setInverse(invers) 
        ## return it
        invers
}
# not necessary because of assignment: make sure the matrix is invertible