## As a first step I tried to understand what cacheMean was doing, which was rather challenging.
## Afterwards I was just following the structure provided by cachemean to set up makeCacheMatrix & cacheSolve.

## makeCacheMatrix is a function factory creating a list of functions which,
## being closures, maintain access to the environment in which they were created.
## This is why the created functions can 'get' the matrix and save the inverse
## matrix and recall it again because they share the same parent (or enclosing)
## environment.  The environment is preserved across function calls.



# see tests on bottom of script



makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        # set sets the vector, any previously saved inverse of a matrix will
        # be set to NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # gets matrix from memory
        get <- function() x
        # save 'solve' in 'inv'
        setinverse <- function(solve) inv <<- solve
        # get 'inv'
        getinverse <- function() inv
        # return a list of functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        # attempt to get inverse matrix
        inv <- x$getinverse()
        # if inv not NULL, return it
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # get matrix
        data <- x$get()
        # solve creates inverse matrix
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

# test =========================================================================
# each of the functions are in the same environment
environment(aMatrix$set)
environment(aMatrix$get)
environment(aMatrix$setinverse)
environment(aMatrix$getinverse)

# test application =============================================================
aMatrix <- makeCacheMatrix(matrix(data = c(1, 2, 4, 2, 5, 10, 0, -1, -1), nrow = 3))
aMatrix$set(matrix(data = c(1, 2, 4, 2, 5, 10, 0, -1, -1), nrow = 3))      # reset value with a new vector
cacheSolve(aMatrix)      # first calculation
# call again to see if cached data is being used
cacheSolve(aMatrix)      # message 'getting cached data'
aMatrix$getinverse()       # test getinverse