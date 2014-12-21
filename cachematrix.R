## The following pair of functions define a special matrix object that is capable to cache its inverse.
## It is assumed that the provided matrix is invertible.

## The first function creates a special matrix object that can cache its inverse. 
## It is simply a list contating the following four functions:
##    1. set the matrix
##    2. get the matrix
##    3. set the inverse of the matrix
##    4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}

## The second function provides the inverse of the special matrix created in the first function.  
## it first checks to see if the inverse has already been calculated. If so, it uses the cached inverse.
## Otherwise, it calculates the inverse and sets the cached inverse matrix.

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        m <- solve(x$get(), ...)
        x$setmatrix(m)
        m
}
