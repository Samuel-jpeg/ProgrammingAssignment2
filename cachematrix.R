## Put comments here that give an overall description of what your
## functions do

#MakeCacheMatrix creates a special matrix which allows you to set and get the
#matrix, aswell as set and get the inverse of the matrix.
#CacheSolve checks to see if an inverse of the matrix has already been
#calculated. If it has it returns the cached inverse. If not it calculates the
#inverse and "prints" it to the screen/ returns the inverse.

## Write a short comment describing this function
#Creats two variables x & y which are stored in a seperate environment from
#the global environement. The inverse of the matrix, if previously calculated
#is stored here.


makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(z) inverse <<- z
        getinverse <- function() inverse
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
#This function checks to see if the inverse of the matrix has previously been
#calculated. If it has it returns the inverse. If the inverse has not been
#calculated then it is is calculated, returned at the end of the function, 
#and stored in the cached variable x.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("Getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(a = data, ...)
        x$setinverse(inverse)
        inverse
}
