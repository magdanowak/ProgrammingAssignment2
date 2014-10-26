## Functions makeCacheMatrix and cacheSolve allow you 
## to cache matrix inverse for invertible matrices. 

## makeCacheMatrix: creates a special object 
## that can cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL

	## function that allows you to set a new matrix
	## and clears cached inverse
      set <- function(y) {
            x <<- y
            inv <<- NULL 
      }

	## function that allows you to retrieve the matrix
	get <- function() x

	## function that allows you to set the inverse 
      setSolve <- function(solve) inv <<- solve
	
	## function that allows you to retrieve cached inverse
      getSolve <- function() inv

	## returns a list of functions to set&get the matrix, 
	## and to cache&retrieve its inverse

      list(set = set, 			
	     get = get,
           setSolve = setSolve,
           getSolve = getSolve)
}

## cacheSolve: computes the inverse of a matrix 
## or retrieves the cached inverse

cacheSolve <- function(x, ...) {
      inv <- x$getSolve() 

	## if cached inverse exists, retrieve it
      if(!is.null(inv)) { 
            message("getting cached data")
            return(inv)
      }
	
	## if cached inverse doesn't exist, 
	## calculate and cache it
      data <- x$get()
      inv <- solve(data, ...)
      x$setSolve(inv)
      inv
}