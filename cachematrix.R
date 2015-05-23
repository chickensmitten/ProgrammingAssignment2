## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Following the example shown in "Example: Caching the Mean of a Vector"
## The difference is just that the function is set for inverse instead of mean.
makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function
## Return the inverse of makeCacheMatrix().
## if function returns the inverse from cache and skips calculation
## if it has been calculated.
## else, it will calculate for the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv = x$getinv()
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        cacheinverse.data = x$get()
        inv = solve(cacheinverse.data, ...)
        x$setinv(inv)
        return(inv)
}




