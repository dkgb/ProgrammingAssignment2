## This piece of code intends to compute inverse of a matrix using 2 functions makeCaheMatrix and cacheSolve 
## note : it is assumed that the input matrix is invertible

## makeCaheMatrix  sets up a matrix for inverse calculations 
## cacheSolve computes the inverse or retrieves it from cache memory.


## This funciton makeCacheMatrix takes a matrix input 'x' and contains a list of functions within


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(invmat) inv <<- invmat
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## this function returns the inverse of a matrix either from the cache ( if it exists) else using solve

cacheSolve <- function(x, ...) {
        
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
