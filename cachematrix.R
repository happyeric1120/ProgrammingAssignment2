## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# The "makeCacheMatrix" is like a class in other object oriented language.
# However, it will return a list of functions, since list can only contain
# the same class (type) of data.
# The list of functions which can be called are:
# 1. set: it will set the matrix x inside the makeCacheMatrix
# 2. get: return the matrix x
# 3. setInvMat: set the inverse matrix of x
# 4. getInvMat: get the inverse matrix of x
# 5. checkInvMat: check the correctness of inverse matrix according to x
# The "checkInvMat" function will also call "checkIdentityMatrix" to 
# verify the multiplication of x and its inverse matrix is the identity
# matrix. Also, the checkInvMat provides the option to generate the messages
# of the result.

makeCacheMatrix <- function(x = matrix()) {
    # Initialize the invMat as NULL
    invMat <- NULL
    # set the matrix x inside the makeCacheMatrix
    set <- function(y) {
        x <<- y
        invMat <<- NULL
    }
    # get the matrix x
    get <- function() x
    # set the inverse matrix of x
    setInvMat <- function(invM) invMat <<- invM
    # get the inverse matrix of x
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
            print(nrow(x))
            print(nrow(invMat))
            # Check the dimension of x and its inverse matrix
            if (nrow(x) != nrow(invMat) | ncol(x) != ncol(invMat)) {
                flag <- 1
            }
            # Check current inverse matrix is true inverse matrix for the matrix
            else if (checkIdentityMatrix(x%*%invMat)) {
                flag <- 3
                # if the inverse matrix of x is correct, return TRUE
                # Otherwise, return FALSE at the end.
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
                # diagonal elements must be 1 
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

# The cacheSolve function will take the lists generated from makeCacheMatrix as
# argument. It will first check if the inverse matrix is existed. If not, it will
# calculate the inverse matrix and save it back to the makeCacheMatrix. If the 
# inverse matrix is existed, it will first verify the inverse matrix by using the
# function makeCacheMatrix$checkInvMat(). If the inverse matrix is correct, it will
# just return the inverse matrix. Otherwise, it will re-calculate the inverse matrix.


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
    # First, get the matrix and store in a variable called data
    data <- x$get()
    
    # The matrix needs to be square matrix and the determinant has to be 
    # non-zeor to be a inversible matrix.
    
    # Check if it is a square matrix
    if (nrow(data) == ncol(data)) {
        # Check if this matrix is inversible (determinant != 0)
        if (det(data) != 0) {
            invMat <- solve(data, ...)
        }
    }
    
    # set inverse matrix back by using setInvMat function
    x$setInvMat(invMat)
    # return the invMat
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