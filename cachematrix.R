# makeCacheMatrix; creates a special matrix


makeCacheMatrix <- function(x = matrix()) {
    #Store result
    invertX <- NULL
   
    
    set <- function(y) {
        x <<- y
        invertX <<- NULL
    }
    
    get <- function() x
    #Set and get the inversed matrix
    setInverse <- function(inverse) invertX <<- inverse
    getInverse <- function() invertX
    

    #return list
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

# cacheSolve; calculates the inverse of a matrix.
# If matix is calculates, it will return the already existing matrix

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    invertX <- x$getInverse()
    if (!is.null(invertX)) {
        message("Getting cached inverse matrix")
        return(invertX)
    } else {
        invertX <- solve(x$get())
        x$setInverse(invertX)
        return(invertX)
    }
}
