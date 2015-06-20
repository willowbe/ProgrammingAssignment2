## these two functions cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix" that can 
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## creates a square invertable matrix.
    
        i <- NULL 
    ## sets the default value of i to NULL
    
#        y <- NULL 
    ## sets the default value of y to NULL
        
    setmatrix <- function(y) {
        x <<- y    
    ## substitutes the matrix x with y (the input) 
        i <<- NULL 
    ## sets the value of i (the matrix inverse after cacheSolve
    ## is used) back to NULL
    
    }
    
    getmatrix <- function() x  
## returns the matrix x stored in makeCacheMatrix
    
    setinverse <- function(inverse) i <<- inverse
## stores the value of the input in a variable i 
    
    getinverse <- function() i
## returns the value of the input into the variable i
    
     list(setmatrix = setmatrix, getmatrix = getmatrix,
      setinverse = setinverse, getinverse = getinverse)
## stores the four functions created in makeCacheMatrix

}


## cacheSolve is a function that returns a matrix 
## that is the inverse of the matrix returned by makeCacheMatrix

## the input is the list of functions created in makeCacheMatrix

## if the inverse has already been calculated and the matrix has not changed 
## it retrieves the inverse from the cache directly

cacheSolve <- function(x, ...) {

    i <- x$getinverse()
    
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
## verifies the value i exists and is not NULL. 

## if it exists in memory, returns a message stating "getting cached data" 
## and the value i   

    data <- x$getmatrix()
## gets the matrix stored with makeCacheMatrix

    i <- solve(data,...)
## computes the inverse of the matrix 

    x$setinverse(i)
## stores the inverse matrix in the cache

    i
}

## TEST THE CODE ##

a <- matrix(c(4,3,3,2), nrow = 2, ncol = 2)
## creates a 2x2 invertable matrix

b <- makeCacheMatrix(a)
## passes the 'a' matrix to the makeCacheMatrix code

cacheSolve(b)
## passes 'b' (the four functions created by makeCacheMatrix)
## to cacheSolve

## inverse of matrix 'a' should be:
##     [,1] [,2]
## [1,]  -2   3
## [2,]   3  -4

## reenter cacheSolve to test what happens when the inverse 
## matrix already exists (should return message "getting cached 
## data" and then return the same inverse matrix as above)
