## The objective it create a function that caches the inverse of a matrix rather than having to repeatedly compute inverses it recognises.
## The functions below together calculate matrix inverse and caches the result, allowing it to immediately call up the results whenever it 
## recognises it.

## The function below, makeCacheMatrix creates a special matrix that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The function below,cacheSolve calculates the inverse of the invertible matrix if it doesn't recognise the inverse. If it does it retrives the inverse 
## from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}