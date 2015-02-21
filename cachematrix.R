## Put comments here that give an overall description of what your
## functions do

## Wraps an R matrixand allows to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	solved <- NULL

	set <- function(y) {
		x <<- y
        solved <<- NULL
    }
        
    get <- function() x
    
    setSolved <- function(s) solved <<- s
        
    getSolved <- function() solved
        
    list(set = set, get = get, setSolved = setSolved, getSolved = getSolved)
}


## checks whether the inverse (= solved) is cached on the cacheMatrix
## object and returns it.
## Otherwise, computes the inverse, caches and returns it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    solved <- x$getSolved()
    
    if(!is.null(solved)) {
    	message("getting cached data")
        return(solved)
    }
    data <- x$get()
    solved <- solve(data, ...)
    x$setSolved(solved)
    solved
}
