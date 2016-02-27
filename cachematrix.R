## These two functions together aim to cache the inverse of a matrix.
## Creating a cache of a matrix could be valuable if the inverse matrix
## will be used repeatedly.

## makeCacheMatrix takes a matrix and returns a list of functions
## 1. set will alter the original stored matrix
## 2. get will retrieve the original stored matrix
## 3. setinverse calculates the inverse matrix
## 4. getinverse returns the stored inverse matrix

makeCacheMatrix <- function(A = matrix()) {
    A_1 <- NULL
    set <- function(y) {
        A <<- y
        A_1 <<- NULL
    }
    get <- function(){A}
    setinverse <- function(solve){A_1 <<- solve}
    getinverse <- function(){A_1}
    list(set = set,  
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse) 
}

## This function will either return the stored inverse matrix
## or calculate the inverse matrix and store it for later use.

cacheSolve <- function(A, ...) {
    ## Return a matrix that is the inverse of 'x'
    A_1 <- A$getinverse()
    if(!is.null(A_1)) {
        message("getting cached data")
        return(A_1)
    }
    matrixA <- A$get()
    A_1 <- solve(matrixA, ...)
    A$setinverse(A_1)
    A_1
}
