## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Cache for the inverse matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL  # Reset the cache when matrix changes
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
    }
    
m <- matrix(c(1, 2, 3, 4), 2, 2)
cm <- makeCacheMatrix(m)
cm$get()  # Retrieve cached inverse

inv <- cacheSolve(cm)  # Compute the inverse
inv                     # Display the inverse
cacheSolve(cm)  # Compute inverse

cm$set(matrix(c(5, 6, 7, 8), 2, 2))  # Change the matrix
cm$get()                             # Retrieve the new matrix
cacheSolve(cm)                       # Compute the new inverse

