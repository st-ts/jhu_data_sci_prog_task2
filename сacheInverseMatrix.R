## The functions below cache an inverse matrix of
## a given matrix X and retrieve it later. If no 
## inverse exists, it is computed and cached.

## A function that creates a list with functions
## to make and cache matrix

makeCacheMatrix <- function(x = matrix()) {
    ## this function takes a matrix (assumed
    ## to be invertible) and returns a list of 
    ## fuctions to set and get matrix, and to
    ## set and get the inverse
    inv_mat <- NULL
    set <- function(y) {
        x <<- y
        inv_mat <<- NULL
    }
    get <- function() x
    set_inv <- function(inv) inv_mat <<- inv
    get_inv <- function() inv_mat 
    list(set = set, get = get, set_inv = set_inv,
         get_inv = get_inv)
}

## A function to compute inverse of a matrix or takes
## it from cached data

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_mat <- x$get_inv
    if (!is.null(inv_mat)) {
        message("here's the cached inverse")
        return(inv_mat)
    }
    mat <- x$get()
    inv_mat <- solve(mat, ...)
    x$set_inv(inv_mat)
    inv_mat
}