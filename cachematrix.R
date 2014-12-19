## Programming Assignment 2 - Cache/Retreive the inverse of a matrix 'x'

makeCacheMatrix <- function(x = matrix()) {
	## Cache the inverse of a matrix 'x'
	## Creates a cache of the inverse of the matrix and methods to retrieve the cached values,
	##     to eliminate overhead in re-processing the intensive matrix-inversion if it has already been performed
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
    }
		
	## get gets the matrix
	## getInverse gets the caches values
	## set sets the original data
	## setInverse uses the 'solve' function to create the matrix inverse
	get <- function() x
	setInverse <- function(solve) m <<- solve
	getInverse <- function() m
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	## Tries to get a cached version for the data - if it exists, use it and return it rather than re-calc
	##    if cached version does not exist, get the data, calc the inverse using 'solve', and store the results into cache (also return the result)

	m <- x$getInverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data)
	x$setInverse(m)
	m
}
