## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache 
## its inverse.
makeCacheMatrix <- function(X = matrix()) {
    inv <- NULL
    # if matrix is not set, settig a matrix
    set <- function(y){
        X <<- y
        inv <<- NULL
    }
    
    # getting matrix
    get <- function() X
    
    # setting and getting inverse of the matrix
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    
    list(set=set, get=get,
         setinv=setinv, 
         getinv=getinv)
}


## Write a short comment describing this function

## expected input: square invertible matrix
## expected output: matrix that is the inverse of 'x'


cacheSolve <- function(X, ...) {
    
    # checking if matrix is squared
#    if (dim(X)[1] != dim(X[2])){
#       print("matrix is not square!!")
#        return
#    }
    
    inv <- X$getinv()
    
    #checking if invers is done 
    if(!is.null(inv)){
        message("getting cached matrix")
        return(inv)
    }
    data <- X$get()
    inv <- solve(data, ...)
    X$setinv(inv)
    inv
    
    
}
cachemean(makeVector(1:10))
cacheSolve(makeCacheMatrix(z))
