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
        message("getting cached data")
        return(invMat)
    }
    
    # reset inverse matrix if the inverse matrix is not existed or incorrect
    invMat <- NULL
    
    data <- x$get()
    # Check if it is a square matrix
    if (nrow(data) == ncol(data)) {
        # Check if this matrix is inversible (determinant != 0)
        if (det(data) != 0) {
            invMat <- solve(data, ...)
        }
    }
    
    x$setInvMat(invMat)
    invMat 
}


a <- rbind(c(4,2), c(3,9))
testMat <- makeCacheMatrix(a)

cacheSolve(testMat)

