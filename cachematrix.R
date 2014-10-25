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
    checkInvMat <- function(showMeg=FALSE) {
        # flag variable is for determining the type of messages
        flag <- 0
        # boolean variable is to return if the inverse matrix is correct
        boolean <- FALSE
        if (!is.null(invMat)) {
            # Since when the matrix x is set, the invMat will be
            # reset to NULL. The rest of the condition is to prevent
            # that the setInvMat function is used and the invMat is
            # set to a wrong inverse matrix for the x
            
            # Check the dimension of x and its inverse matrix
            if (nrow(x) != nrow(invMat) || ncol(x) != ncol(invMat)) {
                flage <- 1
            }
            # Check current inverse matrix is true inverse matrix for the matrix
            else if (checkIdentityMatrix(x%*%invMat)) {
                flag <- 3
                boolean <- TRUE
            } 
            else {
                flag <- 2
            }
        }
        if (showMeg) {
            if (flag == 0)
                message("The inverse matrix is missing, please run cacheSolve to update!")
            if (flag == 1)
                message("The inverse matrix is incorrect (The dimension is wrong), please run cacheSolve to update!\n")
            if (flag == 2)
                message("The inverse matrix is incorrect (incorrect inverse matrix), please run cacheSolve to update!\n")
            if (flag == 3)
                message("The inverse matrix is correct, no update needed!\n")
        }
        return(boolean)

        
    }
    # This function is to check if a matrix is a identity matrix.
    checkIdentityMatrix <- function(matrix) {
        boolean <- FALSE
        for (i in seq_len(nrow(matrix))) {
            for (j in seq_len(ncol(matrix))) {
                
                if (i == j && matrix[i,j] !=1)
                    return(boolean)
                if (i!=j && matrix[i,j] !=0)
                    return(boolean)
            }
        }
        boolean <- TRUE
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
        # Run the x$checkInvMat function to verify the correctness of inverse matrix
        if (x$checkInvMat()) {
            # The inverse matrix is correct, return it.
            message("getting cached data")
            return(invMat)
        }
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

# The belows are the testing codes, which can test these two functions
# are successfully working.
a <- rbind(c(4,3), c(3,2))
testMat <- makeCacheMatrix(a)
cacheSolve(testMat)

# Show and check inverse matrix
print("The matrix is:")
print(testMat$get())
print("Its inverse matrix is")
print(testMat$getInvMat())
testMat$checkInvMat(showMeg=TRUE)

# Create a wrong dimension matrix for inverse matrix
print("Set a wrong dimension matrix as the inverse matrix")
b <- rbind(c(1,2,3), c(0,1,4), c(5,6,0))
testMat$setInvMat(b)
print("The wrong dimension matrix is: ")
print(testMat$getInvMat())
testMat$checkInvMat(showMeg=TRUE)

# Create a wrong inverse matrix
print("Set a wrong matrix as the inverse matrix")
c <- rbind(c(1,2), c(3,4))
testMat$setInvMat(c)
print("The wrong dimension matrix is: ")
print(testMat$getInvMat())
testMat$checkInvMat(showMeg=TRUE)

# Running cacheSolve to update inverse matrix
print("Running cacheSolve to update the inverse matrix")
cacheSolve(testMat)
print("The updated inverse matrix is")
print(testMat$getInvMat())
testMat$checkInvMat(showMeg=TRUE)