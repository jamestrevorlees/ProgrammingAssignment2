
## First step is to create aspecial object that can cache it's inverse to prevent repeating the calculation unecessarily
## Created function for caching called makeCacheMatrix
## makeCacheMatrix creates a list that stores  the functions that are used to set the value of the matrix 'x', get the value of the matrix 'x',... 
## ...set the value of the inverse matrix if it has already been calculated and then finally get the value of the inverse matrix.   

makeCacheMatrix <- function(x = matrix()) {
	
	m <- NULL
	
	set <- function(y) {
		x <<- y
		m <<- NULL
		
	}
	
	get <- function() {
		x
	}
	
	set_inverse <- function(inverse) {
		
		m <<- inverse
		
	}
	
	get_inverse <- function () {
		
		m
		
	}
	
	list( set = set, get = get , set_inverse = set_inverse, get_inverse = get_inverse)
	
	
}


## create function called cacheSolve
## cacheSolve checks to see if the inverse matrix of x has already been created, if so it gets the inverse from the cache and skips the computation. 
## If not, it calculates the inverse of the matrix data and sets the value of the inverse in the cache via the set_inverse function.


cacheSolve <- function(x, ...) {
	
        ## Return a matrix that is the inverse of 'x'
        
		m <- x$get_inverse() {
		
			if(!is.null(m)) {
			
			message("getting cached data")
			return(m)
			
			}
		
		}
		data <- x$get()
		
		##for the purposes of this exercise, as per assignment text, assumption = matrix supplied is always inverible.
		
		m <- solve(data, ...)
		x$set_inverse(m)
		m

}
