## There are two functions in this file, makeCacheMatrix and cacheSolve.
## These two functions will work together to provide a less costly method
## of computing the inverse of a matrix.


## The makeCacheMatrix function will create a "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    a <- NULL
    set <- function(y) {
        x <<- y
        a <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) a <<- inverse
    getinverse <- function() a
    list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## The cacheSolve function will compute the inverse of the matrix
## returned by the first function. If the inverse has already been computed,
## then this function will pull the inversed matrix from the cache

cacheSolve <- function(x, ...) {
    a <- x$getinverse
    if(is.null(a)) {
        message("getting cached data")
        return(a)
    }
    data <- x$get()
    a <- solve(data,...)
    x$setinverse
    a
}

## Testing the result
matrixtest <- matrix(rnorm(16),4,4)
matrixtest1 <- makeCacheMatrix(matrixtest)
cacheSolve(matrixtest1)