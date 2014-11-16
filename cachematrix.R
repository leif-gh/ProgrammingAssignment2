## makeCacheMatrix takes as input a matrix and creates functions for 
## handling the matrix while cacheSolve retrieves its inverse from cache
## or calculates it and caches it through the makeCacheMatrix function.

## makeCacheMatrix takes as input a matrix and creates a list of four 
## functions set, get, set_inv, get_inv where set and get implement 
## functionality to set and retrieve the matrix while set_inv and get_inv 
## does the same for the inverse matrix.

makeCacheMatrix <- function(x = matrix()) { 
	m <- NULL

	set <- function(y)	{
		x <<- y
		m <<- NULL
	}

	get <- function() x
	
	set_inv <- function(m) m <<- solve(x)

	get_inv <- function() m

	list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)

}


## cacheSolve takes as input a matrix, checks if the inverse has been
## calculated. If so it returns the inverse with a message that it is
## retrieved from cache. If not it calculates the inverse, sets it in
## cache via function in makeCacheMatrix and returns it


cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'

	m <-x$get_inv()

	if(!is.null(m))	{
		message("getting cached data")
		return(m)
	}

	data <- x$get()
	m <- solve(data, ...)
	x$set_inv(m)

	m
}
