##The makeCacheMatrix function creates a special matrix in cache and with set of functions
##to set and retrieve the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    
    setinverse <- function(inverse) m <<-inverse
    getinverse <- function() m
    list(set=set,get=get, setinverse=setinverse,getinverse=getinverse)

}


## The cacheSolve function calculates the inverse of the matrix. 
##If the matrix inverse is already calculated, then it takes it from the cache,
##else computes and puts it to cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <-x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
