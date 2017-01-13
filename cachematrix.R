##The below two functions creates a Cache Matrix and computes the inverse of a matrix.

## This function stores the matrix created and also returns the matrix when called.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
  	set <- function(y) {
    	x <<- y
    	m <<- NULL
  	}
  	get <- function() x
  	setMatrix <- function(solve) m <<- solve
  	getMatrix <- function() m
  	list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}


## This function computes the inverse of the matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getMatrix()
  	if(!is.null(m)) {
    	message("getting cached data")
    	return(m)
  	}	
  	matrix <- x$get()
  	m <- solve(matrix, ...)
  	x$setMatrix(m)
  	m
}
