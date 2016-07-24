## I have created a function makeCacheMatrix that contains a variable to cache the inverse. It returns a list object that contains
## four functions namely,
## set() : to set the matrix. It has input argument that is the matrix.
## get() : It returns the matrix
## setinv() : It has an input argument as a matrix (inverse calculated). It sets the inverse to the variable inv (for caching)
## getinv() : It returns the cached inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
		
	get <- function() {x}

	setinv <- function(inverse) {
		inv <<- inverse	
	}

	getinv <- function() { inv }

	list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The cacheSolve function takes the object returned by makeCacheMatrix as input argument. It retrieves the cached inverse an checks if it is not null, in which case, the inverse 
## had already been calculated. The function then returns the cached inverse matrix and also prints "getting cached data".
## Else, the inverse is calculated using solve() in R and set to the cache using setinv() function. 

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'

	inverseMatrix <- x$getinv()
	
	if(!is.null(inverseMatrix)) {
		print("getting cached data")
		return(inverseMatrix)
	}	
		
	matrixData <- x$get()
	
	inverseMatrix <- solve(matrixData)
	
	x$setinv(inverseMatrix)
	
	inverseMatrix
}
