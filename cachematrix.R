## These two functions can be used to calculate and store the inverse of a 
## matrix. The first function "makeCacheMatrix" contain all needed to set the
## values of a matrix and get these back when needed, and the same functions for
## setting and getting the inverse of the matrix.The second function 
## "chacheSolve" can do two things, either it can retrieve a cached value of the
## inverse, or if this value does not already exist, it creates the inverse and
## sets the inverse in the previous function. Allowing the retrieval of the 
## cached inverse.



# This following function  ceates a "special vector"/list containing functions
# to set the values of a matrix and the inverse of this matrix, as well as the 
# functions to get both of these values.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL                         # "i" (for inverse) starts as null
        set <- function(y) {              # function to set the value manually  
                x <<- y
                i <<- NULL      
        }
        get <- function() x             # function that gets the input value
        setinv <- function(inverse) i <<- inverse   # sets the inverse value
        getinv <- function() i                      # gets the inverse value
        list(set = set, get = get,               # creates a list with all parts
             setinv = setinv,
             getinv = getinv)
}




# The following function can both cache a stored inverse of a matrix, or 
# calculate the value and then store it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()          # if the inverse exists already and is 
        if(!is.null(i)) {        # not null, the value and message is printed
                message("getting cached data")
                return(i)
        }
        data <- x$get()                  # If it does not already exist, it is
        i <- solve(data, ...)            # created using the solve function, and
        x$setinv(i)                      # this value is set to the function.
        i                                # The value is then printed.
}


