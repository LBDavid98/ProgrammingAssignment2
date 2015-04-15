## These functions demonstrate the utilization of scoping in R
## by caching a matrix and its inverse within a single object.

## This function will create a cached matrix and the ability to store
## its inverse. It allows the end user and related functions to retrieve
## and set the values for the cached matrix and its inverse. Changing
## the matrix using the $set function will reset the inverse matrix to Null.

makeCacheMatrix <- function(x = matrix()) {
	mInv <- NULL
	
	get <- function() return(x)
	set <- function(z = matrix()) {
		x <<- z
		mInv <<- NULL
	}
	setmInv <- function(inverse) mInv <<- inverse
	getmInv <- function() return(mInv)
	list(get=get, set=set, 
		setmInv=setmInv, getmInv=getmInv)
}


## This function checks the cached matrix for a cached inverse and,
## if it finds one, returns it. If no inverse matrix is cached it will
## create one.

cacheSolve <- function(x, ...) {
	mInv <- x$getmInv()
	if(!is.null(mInv)) {
		message("Getting cached inverse matrix")
		return (mInv)
	}
      mInv <- solve(x$get())
	x$setmInv(mInv)
	mInv
}
