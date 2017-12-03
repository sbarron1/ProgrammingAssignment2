##functions to store a matrix (and it's inverse) and to solve the inverse or
##retrieve it if alreadt cachd

##makeCacheMatrix stores a matrix and can  cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
    
}



## cacheSolve gets the inverse of a cacheable matrix. if the matrixs inverse has
## already been cached, the contents of the cache is returned 
## rather then recalculating the inverse 

cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}

