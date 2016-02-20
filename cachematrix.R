## I want a function to store a matrix and its inverse, 
## with methods to get and set the inverse.
## I want another function to determine the inverse of the matrix.
## If the inverse is already cached, it shall used the cached value and return it. 
## If not, it shall calculate it, store it in the cache and return it.

## create a matrix object with functions (think: methods) to get and set its inverse

makeCacheMatrix <- function(x = matrix()) {
        invers <- NULL 
        ## if matrix gets new values, invers is emptied
        setMatrix <- function(y) {
                x <<- y
                invers <<- NULL
        }        
        getMatrix <- function(){x}
        setInverse <-function(newValue){invers <<- newValue}
        getInverse <- function(){invers}   
        list(getMatrix=getMatrix,
             setMatrix=setMatrix,
             setInverse=setInverse,
             getInverse=getInverse)        
}

## return the inverse of matrix x, or calculate and return if cache is empty

cacheSolve <- function(x, ...) {
        ## is the inverse cached?        
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
        
## to do: not necessary because assignment says so: 
## make sure the matrix is invertible before trying.
## nrow(mymatrix)==ncol(mymatrix)        
        
}
