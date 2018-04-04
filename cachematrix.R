## cachematrix.R includes two functions: makeCacheMatrix and cacheSolve.
## cacheSolve calcualtes the inverse of a matrix, by checking firstly whether 
## the inverse has been calculated or not.

## makeCacheMatrix creates a list, which includes four functions.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve calcualtes the inverse of a matrix, by checking firstly whether 
## the inverse has been calculated or not.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
