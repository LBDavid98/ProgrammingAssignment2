## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	mInv <- x$getmInv()
	if(!is.null(mInv)) {
		message("getting cached inverse matrix")
		return (mInv)
	}
      mInv <- solve(x$get())
	x$setmInv(mInv)
	mInv
}
