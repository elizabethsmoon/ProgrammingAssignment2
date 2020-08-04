## The following functions will create a special "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {  ##defining the argument
        inv <- NULL                     ##holds value of matrix inverse
        set <- function(y){             ##defines set function to assign new
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(solveMatrix) inv <<- solveMatrix  ##assigns value of inv in parent environment
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function will compute the inverse of the matrix that is returned by the makeCacheMatrix
## if the matrix has not changed and inv has been calculated, the function will retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}