## To make matrix inversion faster, this function not only solves (takes inverse)
## but also caches the result of an inversion.

## Returns a special list keeping toBeInvertedMatrix and cache of its inverse.
## There are getter and setter methods for the toBeInvertedMatrix itself and
## also it contains getter and setter methods for its cached inverse.
makeCacheMatrix <- function(x = matrix()) {
        soln <- NULL
        set <- function(y) {
                x <<- y
                soln <<- NULL
        }
        get <- function() x
        setSolve <- function(solution) soln <<- solution
        getSolve <- function() soln
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)  
}

## Returns the inverse of its argument, from cache if possible,
## by matrix inversion if not possible.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        myInverse <- x$getSolve()
        if(!is.null(myInverse)) {
                message("getting cached inverse")
                return(myInverse)
        }
        toBeInvertedMatrix <- x$get()
        myInverse <- solve(toBeInvertedMatrix)
        x$setSolve(myInverse)
        myInverse
}
