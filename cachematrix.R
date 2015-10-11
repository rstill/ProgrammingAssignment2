## Programming assigment 2 for Programming in R.
## Function makeCacheMatrix and cacheSolve create
## a means of cacheing the inverse of a matrix in 
## order to reduce the number of computations required
## when conducting matrix operations.  

# makeCacheMatrix creates a matrix and a cache
# to hold its inverse when computed. 
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, 
    	 get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# cacheSolve computes the inverse of a matrix, 
# if required, stored it in the cache, and
# returns it.  If the inverse has already been 
# computed and cached, it returns the already 
# computed inverse.
cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	if (!is.null(i)) {
		message("Getting cached data.")
		return(i)
	}
	m <- x$get()
	i <- solve(m)
	x$setinverse(i)
	i
}
