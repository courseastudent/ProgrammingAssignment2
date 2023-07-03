## Functions find inverse of matrix and cache it.
## Example:
##    mymatrix <- matrix(1:4,2,2)
##    cm <- makeCacheMatrix(mymatrix)
##    mymatrixinverse <- cacheSolve(cm)
##
## mymatrixinverse is the inverse of mymatrix


## makeCacheMatrix creates a list containing a functions to
## 1) set the value of the matrix, 2) get the value of the matrix,
## 3) set the inverse of the matrix 4)get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve takes the output of makeCacheMatrix as input 
## and returns the inverse of the matrix inputed in makeCacheMatrix

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        ## Return a matrix that is the inverse of 'x'
        m
}
