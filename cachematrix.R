# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    # inv will store the cached inverse matrix
    inv <- NULL
    
    # setter for the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # getter for the matrix
    get <- function() x
    
    # setter for the inverse
    setInverse <- function(inverse) inv <<- inverse
    
    # getter for the inverse
    getInverse <- function() inv
    
    # return the matrix with the newly defined functions
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

# cacheSolve: compute the inverse of the matrix; if the inverse is already
# already calculated, it returns the cached inverse.
cacheSolve <- function(x, ...) {
    ## returns a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    
    # if the inverse is already calculated, returns it
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # the inverse is not yet calculated, so let's calculate it
    mat <- x$get()
    inv <- solve(mat, ...)
    
    # cache the inverse
    x$setInverse(inv)
    
    # returns it
    inv
}