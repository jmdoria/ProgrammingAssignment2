## Create a  matrix object that can cache it's inverse
## to save on time-consuming computations


## Creates a matrix object 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(m) {
                x <<- m
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get, 
             setInverse = setInverse,
             getInverse = getInverse)

}

## Assuming the matrix returned by makeCacheMatrix is always invertible
## calculates the inverse if it doensn't exist, on the contrary retrieves
## the inverse from the cache  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {     
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
