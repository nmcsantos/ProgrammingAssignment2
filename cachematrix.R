## The `makeCacheMatrix` creates a special "matrix" object that can cache its inverse. 

## The 'cacheSolve' function calculates the inverse of the matrix created with the 
## `makeCacheMatrix` function. If the inverse has already been calculated 
## it get`s it from the cache and skips the computation. 

## Last update 21/Sep/2014 by NMCS


## The `makeCacheMatrix` takes a square invertible matrix 'x' and
## returns a list containing functions to
##	1.  set the matrix (and reset cached inverse)
##	2.  get the matrix
##	3.  set the inverse matrix
##	4.  get the inverse matrix
## This list is used as the input to 'cacheSolve'.

## Note 1: `makeCacheMatrix` does not calculate the inverse matrix. Just saves it inside: saves 
## the matrix to variable x and its inverse to variable m in scope.

## Note 2: `<<-` assigns a value to an object in an environment different from the current environment. 

makeCacheMatrix <- function(x = matrix()) {		

 		m <- NULL

            set <- function(y) {
			  ## saves matrix and resets cached inverse
                    x <<- y
                    m <<- NULL
            }

            get <- function(){ 
			  x
		}

            setinv <- function(inv){
			  ## saves inverse
			  m <<- inv
		}

            getinv <- function(){
			  m
		}

            list(set = set, get = get, setinv = setinv, getinv = getinv)
}




## The 'cacheSolve' function calculates the inverse of the matrix
## created with the `makeCacheMatrix` function. However, it first checks to see if the
## inverse has already been calculated (and the matrix has not changed). 
## If so, it get`s the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse 
## in the cache via the `setinv` function.

cacheSolve <- function(x, ...) {

            ## get inverse from cache

		m <- x$getinv()

		## if the inverse has already been calculated (and cached)

            if(!is.null(m)) {

			  ## return the cached data and skip the computation
 
                    message("time saver: getting cached data")
                    return(m)
            }

		## otherwise, calculate the inverse

            data <- x$get()
            m <- solve(data, ...)
		
		## and set the value of the inverse matrix in the cache (via the setinv function)

		x$setinv(m)

		## and return the inverse matrix of the input square matrix 'x'

            return(m)       
}
