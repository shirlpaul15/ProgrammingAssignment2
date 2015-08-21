## Put comments here that give an overall description of what your
## functions do

## Functions caching the inverse of a matrix


## Write a short comment describing this function
## A Specialized "Matrix", which consist of list containing a function to-
##   --> set the value of the matrix and get the value of the matrix
##   --> set the value of the inverse matrix and get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}

## Write a short comment describing this function
## Calculates the inverse of the specialized "Matrix" created using the above function
## if the calculation is done before it will skip the calculation part and reuse the cached result

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    m <- x$get()
    i <- solve(m, ...)
    x$setinverse(i)
    i
}