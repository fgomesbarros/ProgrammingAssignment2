## Caching the Inverse of a Matrix
## 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        # inv will be the inverse of the matrix x and it's reseted to NULL 
        #       everytime makeCacheMatrix is called
        inv  <- NULL    
                       
        # It takes an input matrix and saves the input vector
        # It also resets the inverse to NULL when a new object is created
        set  <- function(y) {    
                x <<- y          
                inv <<- NULL                                         
        }
        
        # It returns the matrix x
        get  <- function() x    
        
        # It takes the inverse of the matrix x and saves in the cache
        setinverse  <- function(inverse) inv <<- inverse  
        
        # Returns the inverse of the matrix x 
        getinverse  <- function() inv    
        
        # It creates the list with functions above.
        list(get = get, set = set, setinverse = setinverse, getinverse = getinverse)        
}

## This function computes the inverse of the special "matrix" returned 
##      by makeCacheMatrix above. If the inverse has already been 
##      calculated (and the matrix has not changed), then the 
##      cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        ## It gets the inverse of the matrix x
        inv  <- x$getinverse()
        
        # It tests if exists a valid inverse of matrix x.
        # If it exists, returns a message and the inverse of matrix x.
        if (!is.null(inv)) {
                message("Getting cached data.")
                return(inv)
        }   
        
        # It gets the matrix x
        data  <- x$get()
        
        # It calculates the inverse matrix of x
        inv  <- solve(data)
        
        # Is set the inverse matrix of x in the cache
        x$setinverse(inv)
}