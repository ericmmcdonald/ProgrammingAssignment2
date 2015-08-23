## Matrix inversion is usually a costly computation. The following functions provide possibility of caching the inverse of a matrix rather than compute it repeatedly.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL

	set <-function(y) {
	x <<- y
	inverse <<- NULL
	}

	get <- function() x
	setInverse <- function(newInverse) inverse <<- newInverse
	getInverse <- function() inverse
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse <- x$getInverse()
  
	if(!is.null(inverse)) {
		message("Receiving cached data")
		return(inverse)
	}

	data <- x$get()
	inverse <- solve(data, ...)
	x$setInverse(inverse)
	inverse
}
