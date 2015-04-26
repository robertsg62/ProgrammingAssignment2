## These functions will cache the inverse of a matrix

## This function will create a list containing a function
## that will set and get the matrix; that will set and get
## the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    ## sets m to NULL as default
    m <- NULL 

    #set the value of the matrix
    set <- function(y) {
        ## caches matrix
        x <<- y
        ## set value of m to NULL, if previously used
        m <<- NULL 
    }

    ## gets the value of the inverse
    get <- function() x
    ## calculates the inverse of non-singular matrix via 
    ## the solve function
    setmatrix <- function(solve) m <<- solve
    ## get the inverse
    getmatrix <- function() m

    ## passes the value of the function makeCacheMatrix
    list(set = set, 
      get = get, 
      setmatrix = setmatrix, 
      getmatrix = getmatrix
    )
}

## This function checks if the inverse of the matrix has been
## solved and cached. It will return the cached matrix if it
## has been solved; otherwise, it will solve the matrix and
## return it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()

    ## check to see if cacheSolve has been run before
    if( !is.null(m) ) {
        message("getting cached data")
        return(m)
    }

    ## Run the get function to get the value of the input matrix
    matrix <- x$get()
    ## Compute the value of the inverse of the input matrix
    m <- solve(matrix, ...)
    ## Run the setinverse function on the inverse to 
    ## cache the inverse
    x$setmatrix(m) 
    m ## Return the inverse
}
