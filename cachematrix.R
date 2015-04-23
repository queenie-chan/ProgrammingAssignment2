## makeCacheMatrix creates special object of functions for a matrix.
    ## matrix can be set (write), get (read), 
    ## setinv (write inverse), getinv (read inverse)
## cacheSolve computes the inverse of a matrix via cache, if inverse is cached
    ## otherwise, solves inverse and sets matrix's inverse via makeCacheMatrix


## makeCacheMatrix creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y         # assign the input, y, as x in parent fxn (makeCacheMatrix)
        inv <<- NULL    # resets inverse of x
    }
    get <- function() x # returns x
    setinv <- function(inverse) inv <<- inverse
                        # sets inv in parent fxn as input, inverse
    getinv <- function() inv
                        # returns inv
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
        return(inv)             # return cached inv
    }         
    inv <- solve(x$get(), ...)  # else, calc inverse
    x$setinv(inv)               # set inverse in inputted matrix
    inv                         # return inv
}
