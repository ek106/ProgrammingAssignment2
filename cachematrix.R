## Use lexical scoping to assign the inverse of a matrix, taking advantage
## of caching

## makeCacheMatrix creates a list that:
## sets the value of the matrix (using set)
## gets the value of the matrix (using get)
## sets the value of the inverse of the matrix (using setinverse)
## gets the value of the inverse of the matrix (using getinverse)

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL                         ##initialize inverse as empty
    set <- function(y){ 
        x <<- y                             ## write matrix to environment above
        inverse <<- NULL                    ## inverse is set to environment above as empty
    }
    get <- function() x                     ## read matrix (from cache)
    setinverse <- function(i) inverse <<- i ## calculate the inverse of the matrix
    getinverse <- function() inverse        ## read inverse of matrix (from cache) 
    list(set = set, get = get,              ## output list with functions
         setinverse = setinverse, 
         getinverse = getinverse)
}


## cacheSolve returns the inverse of a matrix.  The first condition checks if 
## the inverse has been set, and if so pulls up the cached value of the inverse
## of the matrix.  If the inverse has not been calcualted, then the function
## uses the setinverse function to calcualte the inverse of the matrix.  This
## function assumes the matrix is always invertible.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()               ## retrieve cached inverse of matrix if exists
    if(!is.null(inverse)){                  ## print inverse if it is in cache with message
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()                         ## retrieve cached matrix
    inverse <- solve(data)                  ## calculate inverse
    x$setinverse(inverse)                   ## set cache inverse
    inverse                                 ## return inverse
}
