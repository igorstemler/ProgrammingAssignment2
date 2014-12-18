## These functions return the inverse of a matrix. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


# This function sets and gets a matrix and a matrix inverse into a list.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # Function to set the value of the matrix and to set the matrix inverse (m) to NULL.
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # Function to get the value of the matrix.
    get <- function() x
    # Function to set the value of the matrix inverse to m.
    setmatrix <- function(matrix) m <<- matrix
    # Function to get the value of the matrix inverse (m).
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


# This function return a matrix that is the inverse of x with new solves or uses the cached matrix inverse if it has already been calculated.
cacheSolve <- function(x, ...) {
    # m gets the value of the cached matrix inverse .
    m <- x$getmatrix()
    # if new matrix isn't set, then the cached data is used.
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # It gets the matrix and solves the matrix inverse.
    data <- x$get()
    m <- solve(data, ...)
    # It sets the matrix inverse to m.
    x$setmatrix(m)
    # It Returns the matrix inverse.
    m
}
