## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## A special "matrix" that allows for caching its inverse
## inv contains the inverse of the input matrix x

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        m <<-NULL
    }
    get <- function() x
    setinverse <- function(invert) inv <<- invert
    getinverse <- function() inv
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## "Solve"s the matrix that is input via the argument x if not already solved.
## If already solved previously then returns inverse from the cache
## If not solved, solves and sets the inverse of the input matrix.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    mtrx <- x$get()
    inv <- solve(mtrx,...)
    x$setinverse(inv)
    inv
    ## Return a matrix that is the inverse of 'x'
}
