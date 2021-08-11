## As per the requirements of the assignment, two functions are written to 
## create a special Matrix object that can cache its inverse. 

## the function below creates a special object that caches the matrix and 
## its inverse. The function returns a list containing functions to set and 
## retrieve the value of the matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        set <- function(y){
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        set_inv <- function (y) inv_x <<- y
        get_inv <- function() inv_x
        list (set = set, set_inv = set_inv,
              get = get, get_inv = get_inv)
}


## The following function calculates the inverse of the matrix object 
## created through makeCacheMatrix function and passed as the argument to
## this function. However, it first checks if the inverse has already been
## calculated, and if it is, it skips the calculation and simply returns
## the already calculated inverse. 

cacheSolve <- function(x, ...) {
        ## Check if the inverse is already calculated and if it is, return it 
        inv_x <- x$get_inv()
        if(!is.null(inv_x)) {
                print("Returning cached inverse of the matrix")
                return(inv_x)
        }
        
        ## This code will only be executed if the inv_x is null (which means it
        ## doesn't exist)
        
        ## compute the inverse of 'x', set its value in cache and return it 
        inv_x <- solve(x$get())
        x$set_inv(inv_x)
        inv_x
        
}
