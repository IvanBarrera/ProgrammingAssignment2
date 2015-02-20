## The following functions (makeCacheMatrix and cacheSolve) enables R to caching
## the inverse of a matrix instead of compute it every time you need.
##
## If you want test both functions (makeCacheMatrix and cacheSolve),
## you can do the following:
##      test_matrix <- matrix(c(1,2,3,0,1,4,5,6,0), 3, 3, TRUE)
##      matrix_obj <- makeCacheMatrix(test_matrix)
##      inv_matrix <- cachesolve(matrix_obj)
##
## If you run the last sentence twice, you will be notified that your
## inverse matrix was previously solved by the message "getting cached data".
##
## makeCacheMatrix (function)
## This function creates a special  matrix object that stores a matrix, its
## inverse, and four 'sub'functions for getting access to the matrix and its inverse.
## The four methods are set, get, setinverse and getinverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve (function)
## The function takes a matrix object as a parameter and uses it's 'sub'functions
## to get access to the matrix itself and its stored inverse.
## The function returns the inverse of the vector if it exists, otherwise the
## function will calculate a new inverse matrix using the matrix object
## and return the new inverse matrix.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
