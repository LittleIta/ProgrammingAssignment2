## These functions optimse the calculation of the inverse of a matrix
## The first time this function the cacheSolve() function is called the
## inverse of the matrix is calculated. Every subsequent call to the function
## retrive the result from cache.

## Usage examples:
## > a$set(matrix(1:4, 2, 2))   
## > cacheSolve(a)
##     [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## >
## > a$set(rbind(c(1, -1/4), c(-1/4, 1)))   
## > cacheSolve(a)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > 
 

makeCacheMatrix <- function(x = matrix()) {
	## This function creates a special "matrix" object that can cache its inverse
	## It returns a list of functions to:
	##		1. Get the value of the matrix
	##		2. Set the value of the matrix
	##		3. Get the value of the inverse matrix
	##		4. Set the value of the inverse matrix
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
		
	## This function computes the inverse of the special "matrix" 
	## it makes use of makeCacheMatrix above. If the inverse has 
	## already been calculated (and the matrix has not changed), 
	## then the cachesolve will retrieve the inverse from the cache.
	m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
