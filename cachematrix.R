## makeCacheMatrix creates special object of functions for a matrix.
    ## functions include: set (write), get (read), 
                        ## setinv (write inverse), or getinv (read inverse)
## cacheSolve calls makeCacheMatrix functions to return the inverse of the matrix
    ## by retrieving the inverse from the cache (if previously cached), 
    ## or solving inverse, setting matrix's inverse via makeCacheMatrix,
    ## and returning the inverse matrix.


## makeCacheMatrix creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y             # assign x in parent fxn as set's input, 'y' 
        inv <<- NULL        # resets inverse of x (until setinv is called)
    }
    get <- function() x     # returns x
    setinv <- function(inverse) inv <<- inverse
                            # assign inv in parent fxn as setinv's input, 'inverse'
    getinv <- function() inv
                            # returns inv
    
    # makeCacheMatrix result is list of functions of a matrix
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve computes inverse of "matrix" from makeCacheMatrix
    # If inverse already calculated, cacheSolve retrieves inverse from cache
    # Else, computes inverse and sets matrix's inverse
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinv()           # read inverse from x
    if (!is.null(inv)) {        # if inv exists, grab cached value
        message("retrieving cached inverse")
        return(inv)             # return cached inverse
    }         
    inv <- solve(x$get(), ...)  # else, calculate inverse
    x$setinv(inv)               # set x's inverse
    inv                         # return (print) inverse of x
}
