# PART 1: Where data is stored
makeVector <- function(x = c()) {
    m <- NULL  # Empty cache
    
    set <- function(y) {
        x <<- y
        m <<- NULL  # Clear existing cache
    }
    
    get <- function() x
    
    setmean <- function(mean_value) {
        m <<- mean_value  # Just stores a number
    }
    
    getmean <- function() m
    
    list(set = set, get = get,
         setmean = setmean, getmean = getmean)
}

cachemean <- function(special_vector) {
    # Check if we already calculated
    m <- special_vector$getmean()
    
    if (!is.null(m)) {
        message("Using cached mean")
        return(m)
    }
    
       data <- special_vector$get()      # Get the numbers
    m <- mean(data)                   # calculation of the mean of the data
    special_vector$setmean(m)         # Save the result
    return(m)                         # Return the mean
}

## makeCacheMatrix: Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    # Initialize the cache as NULL (empty)
    inv <- NULL
    
    # Set function: Updates the matrix and clears the cache
    set <- function(y) {
        x <<- y
        inv <<- NULL  # Clear cache because matrix changed
    }
    
    # Get function: Returns the original matrix
    get <- function() x
    
    # Set inverse function: Stores the calculated inverse in cache
    setinverse <- function(inverse) {
        inv <<- inverse
    }
    
    # Get inverse function: Returns the cached inverse (or NULL if not calculated yet)
    getinverse <- function() inv
    
    # Return a list of all the functions
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

/Computes the inverse of the matrix, using cache if available
cacheSolve <- function(x, ...) {
    # Step 1: Check if inverse is already cached
    inv <- x$getinverse()
    
    # Step 2: If cached value exists, return it with a message
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Step 3: If not cached, calculate the inverse
    data <- x$get()           # Get the original matrix
    inv <- solve(data, ...)   # Calculate inverse using solve()
    
    # Step 4: Cache the calculated inverse for future use
    x$setinverse(inv)
    
    # Step 5: Return the inverse
    inv
}
