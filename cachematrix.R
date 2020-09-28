## Put comments here that give an overall description of what your
## functions do
## This is a function that can save the matrix in cache
## and the second function can call the information
##saved in the cache

## Write a short comment describing this function
## This function create a variable where
## the information about the matrix is saved

makeCacheMatrix <- function(x = matrix()) {
        inversa <- NULL
        set <- function(y){
                x <<- y
                inversa <<- NULL
        }
        get <- function() {x}
        setInversa <- function(inversacalculada) {inversa <<- inversacalculada}
        getInversa <- function() {inversa}
        list(set = set, get = get,
             setInversa = setInversa,
             getInversa = getInversa)
}


## Write a short comment describing this function
## This function can call the information saved
## in cache about the matrix
## This is faster than make the inverse matrix again

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversa <- x$getInversa()
        if(!is.null(inversa)){
                message("getting cache data")
                return(inversa)
        }
        data <- x$get()
        inversa <- solve(data, ...)
        x$setInversa(inversa)
        inversa
}
