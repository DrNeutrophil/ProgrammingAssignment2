## The makecacheMatrix function set a matrix as the input. The matrix object
## can cache it's own objects. 

## set an function where the matrix is the input

makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        
        setMatrix <- function(y){
                x <<- y
                invMatrix <<- NULL
        }   ##set the value of matrix
        getMatrix <- function() x  ##get the value of matrix
        setinv <- function(inverse){
                invMatrix <<- inverse  
        }  ## set the invertible matrix                      
        getinv <- function()invMatrix  ## get the value of the invertible matrix
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setinv = setinv, getinv = getinv)
}


## This function use the output from makeCacheMatrix as input and priduce the
## inverse matrix as the output depending on:
### 1. if the makeCacheMatrix(x)is has no value, it gets the original matrix data 
###     and compute the inverse matrix.
### 2. if the makeCacheMatrix(x)is has been calculated before, it returns the notification
###     and the cached value. 

cacheSolve <- function(x, ...) {
        invMatrix <- x$getinv()
        ## when the value of inverse matrix was computed before
        if(!is.null(invMatrix)){  ##if inverse matrix is not null
                message("Value is found! Getting cached invertable matrix")  ##type notification
                return(invMatrix)
        }
        ## when the value of inverse matrix was never computed
        else if(is.null(invMatrix)){   
                Data <- x$getMatrix() ##get the original matrix
                invMatrix <- solve(Data,...) ##use solve function to inverse the mattix
                x$setinv(invMatrix)  ##set the invertible matrix
                message("The inverse matrix was never computed before") ##type notification
                return(invMatrix)  ## return the inversed matrix
        }
        
} ##get the value of inverted matrix from last function

        ## Return a matrix that is the inverse of 'x'
