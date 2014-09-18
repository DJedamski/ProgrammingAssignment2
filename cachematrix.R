## This function will return a list of functions that will:
# 1. Set the value of a matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the matrix

makeCacheMatrix <- function(x=matrix()) {
	
	# Store the cached inverse matrix
	i <- NULL
	
	# Set the matrix
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	
	# Get the matrix
	get <- function() x
	
	# Set the inverse 
	setinverse <- function(inverse) i <<- inverse
	
	# Get the inverse
	getinverse <- function() i
	
	# Return the matrix with the new functions
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# This function calculates the inverse of the matrix. If the inverse
# is already calculated, it returns the cached matrix.

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	
	# If the inverse is already calculated, return it
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	
	# If it hasn't been calculated, then calculate it
	data <- x$get()
	i <- solve(data, ...)
	
	# Cache the inverse
	x$setinverse(i)
	
	# Return the inverse
	i
}
