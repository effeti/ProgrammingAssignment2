## Put comments here that give an overall description of what your
## functions do
## My functions compute the inverse of a square invertible matrix or 
## retrive the data if the inverse has already been calculated

## The function makeCacheMatrix creates a special "vector": 
## a list containing functions that 1. set the value of the matrix,
## 2. get the value of the matrix (getimat), 3. set the value of the 
## inverse matrix, 4. get the value of the inverse matrix (getimat)

makeCacheMatrix <- function(x = matrix()) {
	im <- NULL
	set <- function(y) {
		x <<- y
		im <<- NULL
	}
	get <- function() x
	setimat <- function(solve) im <<- solve
	getimat <- function() im
	list(set = set, get = get,
		setimat = setimat, 
		getimat = getimat)
}


## The function cacheSolve gets the inverse matrix from the cache, if this
## is already been calculated and then its value is not null. Otherwise it
## computes the inverse of the matrix by applying the function solve() 

cacheSolve <- function(x, ...) {
        	  im <- x$getimat()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)  ## Return a matrix that is the inverse of 'x'
        x$setimat(im)
        im
}
