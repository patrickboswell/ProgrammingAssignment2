## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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