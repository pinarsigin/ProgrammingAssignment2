## To make matrix inversion faster, this function not only solves (takes inverse)
## but also caches the result of an inversion.

## Write a short comment describing this function
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

## Write a short comment describing this function
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
