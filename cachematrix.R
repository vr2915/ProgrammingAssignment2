## Function makeCacheMatrix creates a special matrix that can cache its inverse.
## Function cacheSolve returns the inverse of a special matrix returned by makeCacheMatrix

## 
## Function makeCacheMatrix sets and gets the current matrix and its inverse to/from 
## outside objects x(matrix) and im (inverse of the matrix). 
## It returns a list containing the functions to set and get the matrix and its inverse

makeCacheMatrix <- function(m = matrix()) {
        
        ##Created x and im objects if not yet existing
        if (!exists("x")) x <<- NULL
        if (!exists("im")) im <<- NULL
        
        ##preserve current value of matrix x for comparison with new matrix
        m <- x
        
        ## get and set functions
        setmatrix <- function(y){x <<- y}
        getmatrix <- function() x
        getcurmatrix <- function() m
        setinverse <- function(inv) im <<- inv
        getinverse <- function() im
        
        ## return list
        list(setmatrix=setmatrix,
             getmatrix=getmatrix,
             getcurmatrix=getcurmatrix,
             setinverse=setinverse,
             getinverse=getinverse)
}



## Function CacheSolve accepts matrix x and checks if (1) the inverse of the matrix has not been calculated or 
## cached yet and (2) if the matrix has changed before inversing the matrix. 
## Otherwise, it will return the cached inverse of the matrix
##Example:
##rm(list = setdiff(ls(), lsf.str()))
##d<-matrix(rnorm(1:16),nrow=4,ncol=4)
##f<-matrix(rnorm(11:26),nrow=4,ncol=4)
##cacheSolve(d)
##cacheSolve(d)
##cacheSolve(f)
##cacheSolve(f)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        z <- makeCacheMatrix(m)
        i <- z$getinverse()
        
        cur <- z$getcurmatrix()


        if(length(i)>0 & identical(x,cur)){
                message("Getting cached inversed matrix...")
                return(i)
        }
        else if (length(i)==0){
                message("No cached inversed matrix.")
        }
        else if (!identical(x,cur)){
                message("Matrix changed.")
        }
        
        message("Inversing matrix...")
        i <- solve(x)
        z$setinverse(i)
        z$setmatrix(x)
        i 
        
}
