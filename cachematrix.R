## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {      # this function creat a special matrix object that can cache its inverse
                                                 # the input should be a matrix
        inv <- NULL                              # inv will be the inverse of the special matrix and it's reset to NULL
                                                 # every time makeCacheMatrix is called
        
        set <- function(new_matrix) {            # take an input matrix
                matrix <<- new_matrix            # store the input matrix to the matrix object using superassignment
                inv <<- NULL                     # reset the inverse to NULL
        }
        get <- function() {matrix}               # return the value of the original matrix 
        set_inv <- function(inverse) {inv <<- inverse} 
                                                 # this is called by cacheSolve() during the first access and
                                                 # it will store the 'inverse' of the matrix using superassignment
        
        get_inv <- function() {inv}              # return the cached 'inverse' to cacheSolve() on subsequent accesses
        
        list(set = set, get = get,               # this is accessed every time makeCacheMatrix() is called
             set_inv = set_inv,                  # this is a list of the internal functions
             get_inv = get_inv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # the input is a matrix object created by makeCacheMatrix
        
        inv <- matrix$get_inv()                # access the object and gets its inverse
        
        if(!is.null(inv)){                     # if the inverse was cached (not NULL)
                message("getting cached data") # send this message
                return(inv)                    # return the inverse. "return" ends the function cacheSolve.
        }
        
        data <- matrix$get()                                  
        inv <- solve(data, ...)                # if matrix$get_inv() returned NULL then the calculate the inverse
        matrix$set_inv(inv)                    # store the calculated inverse in the matrix object
        inv                                    # return the inverse of the matrix to the code that called this function
}
