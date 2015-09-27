# Matrix inversion is a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions that are used to create a matix and cache's its inverse.

# makeCacheMatrix creates a list which contains function to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    set <- function(y) {
        x <<- y
        minv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) minv <<- inverse
    getinverse <- function() minv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed or not. If not, it computes the inverse, 
# sets the value in the cache via setinverse function otherwise it gets the result
# from cache.
cacheSolve <- function(x, ...) {
    minv <- x$getinverse()
    if(is.null(minv)) {
        message("computing and caching inverse of matrix")
        data <- x$get()
        minv <- solve(data)
        x$setinverse(minv)
        
    }
    else{
        message("getting cached inverse of matrix")
    }
    minv
}
