## Put comments here that give an overall description of what your
## functions do

## this function creat a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {      # the input should be a matrix
        inv <- NULL                              # inv will be the inverse of the special matrix and it's reset to NULL
                                                 # every time makeCacheMatrix is called
        
        set <- function(new_matrix) {            # take an input matrix
                x <<- new_matrix            # store the input matrix to the matrix object using superassignment
                inv <<- NULL                     # reset the inverse to NULL
        }
        get <- function() {x}               # return the value of the original matrix 
        set_inv <- function(inverse) {inv <<- inverse} 
                                                 # this is called by cacheSolve() during the first access and
                                                 # it will store the 'inverse' of the matrix using superassignment
        
        get_inv <- function() {inv}              # return the cached 'inverse' to cacheSolve() on subsequent accesses
        
        list(set = set, get = get,               # this is accessed every time makeCacheMatrix() is called
             set_inv = set_inv,                  # this is a list of the internal functions
             get_inv = get_inv)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {               # the input is a matrix object created by makeCacheMatrix
        
        inv <- x$get_inv()                # access the object and gets its inverse
        
        if(!is.null(inv)){                     # if the inverse was cached (not NULL)
                message("getting cached data") # send this message
                return(inv)                    # return the inverse. "return" ends the function cacheSolve.
        }
        
        data <- x$get()                                  
        inv <- solve(data, ...)                # if x$get_inv() returned NULL then the calculate the inverse
        x$set_inv(inv)                    # store the calculated inverse in the matrix object
        inv                                    # return the inverse of the matrix to the code that called this function
}
