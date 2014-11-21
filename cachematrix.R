## Here are two functions that can be used to create a special object
## that stores a matrix and, once computed, cache's its inverse.


## This function creates an object (a list) that stores a matrix and 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
    x.inv <- NULL
    set <- function(y) {
        x <<- y
        x.inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) x.inv <<- inv
    getinverse <- function() x.inv
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## This function takes an object created with the function 'makeCacheMatrix'
## and returns the inverse of the matrix in that object.

cacheSolve <- function(x) {
    x.inv <- x$getinverse()
    
    ## if the inverse of the matrix stored in 'x'
    ## was already computed let's just return it
    if (!is.null(x.inv)) {
        message("Getting cached data")
        return(x.inv)
    }
    
    ## if the inverse was not already computed 
    ## let's compute it and cache it in 'x'
    mat <- x$get()
    x.inv <- solve(mat)
    x$setinverse(x.inv)
    x.inv
}

