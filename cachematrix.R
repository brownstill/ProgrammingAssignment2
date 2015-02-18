## 20150215 RProg Assignment 2, R version 3.1.2 (2014-10-31)
## Goal: Write an R function that is able to cache potentially
## time-consuming computations. Take advantage of the scoping rules of the R
## language and how they can be manipulated to preserve state inside of an R
## object.
## Background: Matrix inversion is usually a costly computation and there may
## be some benefit to caching the inverse of a matrix rather than compute it 
## repeatedly.
## New concepts: The <<- operator which can be used to assign a value to an
## object in an environment that is different from the current environment.
## Computing the inverse of a square matrix can be done with the solve function
## in R. For example, if X is a square invertible matrix, then solve(X) returns
## its inverse.
## Assumptions: Assume that the matrix supplied is always invertible.
## Constraints: Works for square matricies only; solve(x) only works for square
## matricies.


## makeCacheMatrix function creates a special "matrix" object that can cache its
## inverse. It creates a special "matrix", which is really a list of 4 functions
## to access and modify the cache.

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize inverse to NULL at first call
        i <- NULL
        ## Function to set a new value of the matrix
        ## Set x and its inverse in the enclosing environment
        ## Reset i to NULL since we are modifying the underlying
        # matrix and the cached value is no longer valid 
        set <- function(y) {
                x <<- y
                i <<- NULL 
        }
        ## Function to get the value of the matrix
        get <- function() x
        
        ## Function to set the value of the inverse of the matrix in the cache
        ## Use the <<- to set the variable in enclosing environment so the value
        ## remains even after the function call completes.
        setinverse <- function(inverse) i <<- inverse
        
        ## Function to get the value of the inverse of the matrix
        getinverse <- function() i
        
        # return value of the makeVector function is a list
        # of functions (and variables if we wish) that we want to expose
        # as public. these are accessed with the $ operator. Any variables
        # declared inside makeCacheMatrix but not exported as part of this list
        # are private...they are inaccessible to any caller of makeVector
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## cacheSolve function computes the inverse of the special "matrix" (cache)
## built using makeCacheMatrix. If the inverse has already been calculated (and
## and the matrix has not changed), then cachesolve retrieves the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Get inverse of matrix from cache
        i <- x$getinverse()
        ## If inverse has already been calculated, return inverse from cache
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## If no inverse returned from cache (or set() has been called to set 
        ## inverse to NULL), compute inverse and set in cache
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

## TEST CODE https://class.coursera.org/rprog-011/forum/thread?thread_id=405,
## Jules Stuifbergen
# Test your code
# source("cachematrix.R")
#
# generate matrix, and the inverse of the matrix.
#size <- 1000 # size of the matrix edge, don't make this too big
#mymatrix <- matrix(rnorm(size^2), nrow=size, ncol=size)
#mymatrix.inverse <- solve(mymatrix)
#
# now solve the matrix via the cache-method
#
#special.matrix   <- makeCacheMatrix(mymatrix)
#
# this should take long, since it's the first go
#special.solved.1 <- cacheSolve(special.matrix)
#
# this should be lightning fast
#special.solved.2 <- cacheSolve(special.matrix)
#
# check if all solved matrices are identical
#identical(mymatrix.inverse, special.solved.1) & identical(mymatrix.inverse, special.solved.2)
#
# should return TRUE
