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

