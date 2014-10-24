## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    invMat <- NULL
    set <- function(y) {
        x <<- y
        invMat <<- NULL
    }
    get <- function() x
    setInvMat <- function(invM) invMat <<- invM
    getInvMat <- function() invMat
    list(set = set, get = get,
         setInvMat = setInvMat,
         getInvMat = getInvMat)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invMat <- x$getInvMat()
    # Check if the inverse of matrix is existed
    if(!is.null(invMat)) {
        # 
        message("getting cached data")
        return(invMat)
    }
    data <- x$get()
    tryCatch ({
        invMat <- solve(data, ...)
    }, error <- function(e) {
        print(paste("MY_ERROR:  ",e))
        return(NA)
    }, warning <- function(w) {
        print(paste("MY_ERROR:  ",w))
        return(NULL)
    }) 
    
    x$setInvMat(invMat)
    invMat 
}
