## This file provides two functions that perform functions to calculate
## the inverse of a user supplied matrix.  The result is cached so that
## calls after the first will quickly return the cached value
##
## NOTE: Per the assignment rules, these functions only support a square
##       invertable matix (N by N).
##
## A unit test program is provided by the 'cacheSolve.UnitTest' function in
## this file.
##
#####################################################################
## Calculate the inverse of a matix and caches the result in case
## it is requested again
##
##  'x' is a matrix
##
##  Return a special "vector", which is really a list containing a function to:
##    - 'set'        -- set the the matrix
##    - 'get'        -- get the the matrix
##    - 'setInverse' -- set the value of the inverse matrix
##    - 'getInverse' -- get the inverse of the matrix
##
makeCacheMatrix <- function(x = matrix())
{
    # Initialize the cached matrix

    cachedMatrix <- NULL
    set <- function(y)
    {
        x <<- y
        cachedMatrix <<- NULL
    }
    get        <- function() x
    setInverse <- function(inverse) cachedMatrix <<- inverse
    getInverse <- function() cachedMatrix

    list(set = set, get = get,
         setinverse = setInverse,
         getinverse = getInverse)
}

#####################################################################
## Get the inverse of a matrix
##
##  'x' is vector of functions calculated by the "makeCacheMatrix" routine
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...)
{
    # See if an inverse has already been calculated

    inverse <- x$getinverse()

    if (!is.null(inverse))
    {
        # Yes, return cached result
        message("getting cached data")
        return(inverse)
    }
    # Retrieve the matrix
    theMatrix <- x$get()

    # Calculate the inverse of the matrix
    inverse <- solve(theMatrix) %*% theMatrix

    # Save off/cache the inverse
    x$setinverse(inverse)

    # Return the inverse as a result
    inverse
}

#####################################################################
## Unit Test
##
##  Simple unit test of the 'cacheSolve' function.   The default is
##  a normalized distribution of numbers in a 7x7 matrix.  You can change
##  this size with the 'matrixSize' parameter
##
##  Returns 'TRUE' if successful
##
cacheSolve.UnitTest <- function(matrixSize = 7)
{
    # Create a 7x7 matrix to operate on.  Random numbers....

    testMatrix <- matrix(as.numeric(runif(matrixSize*matrixSize, 0, 100)),
                         nrow = matrixSize, ncol = matrixSize)

    # Output for comparison later

    print("The matrix is:")
    print(testMatrix)

    # Compute the inverse here for test comparison later

    testInverse <- solve(testMatrix) %*% testMatrix

    ############################################################
    # Create the object that caches the mean of the above vector

    invCacher <- makeCacheMatrix(testMatrix)

    ############################################################
    # Now ask for and print out the inverse

    theInverse <- cacheSolve(invCacher)
    print("The inverse is")
    print(theInverse)

    # Verify

    if (!identical(theInverse, testInverse))
    {
        warning("The matrix inverse returned was not correct")
        return(FALSE)
    }
    # Ask for it again.  It should print out the 'getting cached data'
    # message...

    secondInverse <- cacheSolve(invCacher)

    if (!identical(secondInverse, testInverse))
    {
        warning("The cached matrix inverse returned was not correct")
        return(FALSE)
    }
    print("Success if you see the 'getting cached data' message above this line")
    # Success

    TRUE
}
