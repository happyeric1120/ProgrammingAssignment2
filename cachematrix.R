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
    
    # Check inverse matrix function
    checkInvMat <- function() {
        if (!is.null(invMat)) {
            # Check the dimension of x and its inverse matrix
            if (nrow(x) != nrow(invMat) || ncol(x) != ncol(invMat)) {
                message("The inverse matrix is incorrect, please run cacheSolve to update!")
                return(FALSE)
            }
            # Check current inverse matrix is true inverse matrix for the matrix
            if (checkIdentityMatrix(x%*%invMat)) {
                message("The inverse matrix is correct, no update needed!")
                return(TRUE)
            } else {
                message("The inverse matrix is incorrect, please run cacheSolve to update!")
                return(FALSE)
            }
        }
        message("The inverse matrix is missing, please run cacheSolve to update!")
        return(FALSE)

        
    }
    # This function is to check if a matrix is a identity matrix.
    checkIdentityMatrix <- function(matrix) {
        boolean = FALSE
        for (i in seq_len(nrow(matrix))) {
            for (j in seq_len(ncol(matrix))) {
                
                if (i == j && matrix[i,j] !=1)
                    return(boolean)
                if (i!=j && matrix[i,j] !=0)
                    return(boolean)
            }
        }
        boolean = TRUE
        return(boolean)
    }
    list(set = set, get = get,
         setInvMat = setInvMat,
         getInvMat = getInvMat,
         checkInvMat = checkInvMat)
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
    
    message("New inverse matrix calculated")
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

# The belows are the testing codes
a <- rbind(c(4,3), c(3,2))
testMat <- makeCacheMatrix(a)
cacheSolve(testMat)

# Show and check inverse matrix
print("The matrix is:")
print(testMat$get())
print("Its inverse matrix is")
print(testMat$getInvMat())
testMat$checkInvMat()

b <- rbind(c(1,2,3), c(0,1,4), c(5,6,0))
testMat$set(b)
testMat$checkInvMat()


    