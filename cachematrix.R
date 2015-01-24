## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value from the vector
## 3. set the value of the cashed inverse matrix
## 4. get the value of the cashed inverse matrix

## The below function sets the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse_mat <- NULL 					## Initially assigning 'NULL' to inverse_mat
	set_matrix <- function(y) {			
		x <<- y 					## Setting the matrix 'x'
		inverse_mat <<- NULL
	}
	get_matrix <- function() x 				## Returning matrix 'x'
	set_inverse <- function(solve) inverse_mat <<- solve 	## Cache the value of the inverse_mat 
	get_inverse <- function() inverse_mat 			## Returning inverse
	list(set_matrix = set_matrix, get_matrix = get_matrix,
	     set_inverse = set_inverse,
	     get_inverse = get_inverse)
}


## The following function calculates the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse matrix has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via 
## the 'set_inverse' function.

cacheSolve <- function(x, ...) {
        inverse_mat <- x$get_inverse()				## Getting inverse
	if(!is.null(inverse_mat)) {				## Checking for the presence of inverse
		message("getting cached data")			## Displaying message
		return(inverse_mat)
	}
	data <- x$get_matrix()					## Getting Matrix
	inverse_mat <- solve(data, ...)				## Using solve() to compute inverse matrix
	x$set_inverse(inverse_mat)				## To cache the inverse
	inverse_mat 						## Returning the inverse matrix
}
