# Set of functions that involve creating a matrix that can compute
# and cache its inverse for future retrieval

# Returns list of functions that modify and retrieve
# the data and inverse of the matrix x
makeCacheMatrix <- function(x = matrix()) {
    
    # Initialize the inverse to NULL
    inv <- NULL
    
    # Sets the data of the matrix to a new value passed in
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    
    # Retrieves the data of the cached matrix
    get <- function()
    {
        x
    }
    
    # Sets the inverse of the cached matrix to a new value
    setInverse <- function(inverse)
    {
        inv <<- inverse
    }
    
    # Retrieves the inverse of the cached matrix
    getInverse <- function()
    {
        inv
    }
    
    # Returns the list of functions possible
    list (get = get, set = set, setInverse = setInverse, getInverse = getInverse)
}


# Returns the cached inverse of the cached matrix x
# Computes if there is no cached inverse
cacheSolve <- function(x, ...) {
    
    # Retrieves cached inverse of x
    inv <- x$getInverse()
    
    # Checks if cached inverse exists
    if(!is.null(inv)) {
        message("Getting cached inverse...")
        return(inv)
    }
    
    # Retrieves matrix data if inverse does not exist
    data <- x$get()
    
    # Computes inverse and sets it
    inv <- solve(data)
    x$setInverse(inv)
    inv
}
