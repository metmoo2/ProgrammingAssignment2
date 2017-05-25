# Matrix inversion is usually a costly computation and there
# may be some benefit to caching the inverse of a matrix rather
# than compute it repeatedly. These functions cache the inverse of 
# a matrix.

#makeCacheMatrix creates a list containing a function to

# (1) set the values of the matrix
# (2) get the values of the matrix
# (3) calculate the inverse of the matrix
# (4) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <-- solve(x)
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv=getinv)
}


# The following function calculates returns the inverse of a
# matrix created with the above function. However, it first checks
# to see if the inverse has already been calculated. If so, it gets the
# inverse from the cache and skips the computation. Otherwise, it
# calculates the inverse of the matrix and sets the value
# in the cache via the setmean function. 

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}