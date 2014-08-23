# These two functions together enable caching of the result of inversion 
# of a matrix. This avoids repeated recalulation, e.g. in a loop.

# makeCacheMatrix accepts a matrix as argument and stores it in variable x
# in the global environment.
# And also creates a list of named functions in the global environment.
# 
# It is assumed that matrices are square and are actually invertible. 
# There is no check for non-conforming matrices.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                
        set <- function(y) {      
                x <<- y           
                m <<- NULL      
        }                       
        get <- function() x     
        setinverse <- function(solve) m <<- solve   
        getinverse <- function() m   
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


# cacheSolve returns a matrix that is the inverse of x, first checking
# whether the calculation has already been done and the results saved
# in variable m. If that is so, the calculation is not done and the
# saved result is returned with an information message.
#
# The named functions in a list ceated by makeCacheMatrix are used.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         m <- x$getinverse()        
        if(!is.null(m)) {       
                message("getting cached data")
                return(m)
        }
        data <- x$get()        
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
#
# Thanks to Gregory D. Horne in the Forums for example output of the 
# vector functions.
#
# Referred to Wikipedia on invertible matrices:
#       http://en.wikipedia.org/wiki/Invertible_matrix
#       http://en.wikipedia.org/wiki/Identity_matrix
# 
# Referred to Norman Matloff 'Art of R Programming' 
# on General Matrix Operations
