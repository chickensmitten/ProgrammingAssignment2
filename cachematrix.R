## Put comments here that give an overall description of what your
## functions do

## Help from External Sources
## revise basics of R: http://cran.r-project.org/doc/manuals/R-intro.pdf
## test function and checking: http://masterr.org/r/how-to-cache-a-matrix-inversion-in-r/ 

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
## Follow the second example shown in "Example: Caching the Mean of a Vector"
## Difference is to just return the inverse of makeCacheMatrix() instead of the mean.
## if function returns the inverse from cache then it skips calculation as it has been calculated.
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




