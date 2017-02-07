## Use lexical scoping to assign the inverse of a matrix, taking advantage
## of caching

## makeCacheMatrix creates a list that:
## sets the value of the matrix (using set)
## gets the value of the matrix (using get)
## sets the value of the inverse of the matrix (using setinverse)
## gets the value of the inverse of the matrix (using getinverse)

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(i) inverse <<- i
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns the inverse of a matrix.  The first condition checks if 
## the inverse has been set, and if so pulls up the cached value of the inverse
## of the matrix.  If the inverse has not been calcualted, then the function
## uses the setinverse function to calcualte the inverse of the matrix.  This
## function assumes the matrix is always invertible.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
