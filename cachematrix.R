## Below are two functions that are used to create a special object that stores 
## a numeric matrix and cache's its inverse.

## The first function, makeCacheMatrix creates a special "matrix", which is 
## really a list of functions that

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function (y){
		x <<- y
            inv <<- NULL
	}
	get <- function() x
	setinv <- function (inv2) inv<<-inv2
	getinv <- function () inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)

}

## The following function calculates the inverse of the special "matrix" 
## created with the makeCacheMatrix function. However, it first checks to see 
## if the inverse has already been calculated. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the inverse 
## of the matrix and sets the value of the inverse in the cache via the 
## setinv function.

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if(!is.null(inv)){
		message("getting cached data")
	} else {
		data <- x$get()
		inv <- solve(data, ...)
		x$setinv(inv)
	}
      ## Return a matrix that is the inverse of 'x'
	inv
}