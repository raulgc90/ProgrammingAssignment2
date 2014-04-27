# Functions to create an object that contains a 
# matrix and manage a cached inverse

# Creates a cache matrix object
makeCacheMatrix <- function(x = matrix()) {
		# set the inverse as null
        inverse <- NULL
		# function for setting a new matrix
        set <- function(y) {
                x <<- y
				# set the inverse as null
				# need to be calculated again
                inverse <<- NULL
        }
		# function to get the matrix data 
        get <- function() x
		# function that sets the inverse
        setinverse <- function(inv) inverse <<- inv
        # function that returns the inverse
		getinverse <- function() inverse
		# returns a list with the functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# Returns the inverse of a cache matrix object
cacheSolve <- function(x, ...) {
		# gets the actual inverse value
        inverse <- x$getinverse()
		# if the inverse is not null
        if(!is.null(inverse)) {
                message("getting cached data")
				# returns the cached inverse
                return(inverse)
        }
		# gets the matrix data
        data <- x$get()
		# computes the inverse
        inverse <- solve(data)
		# sets the inverse in the cache matrix object
        x$setinverse(inverse)
		# returns the new inverse
        inverse
}