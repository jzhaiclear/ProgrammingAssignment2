## Put comments here that give an overall description of what your 
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    } 
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(mat, ...) {
        ## Return a matrix that is the inverse of 'mat'
    inv <- mat$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- mat$get()
    inv <- solve(data, ...)
    mat$setinv(inv)
    inv
}
