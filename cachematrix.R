## R Programming - Programming assignment 2: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly


## This function creates a special "matrix" object that can cache its inverse.
## It contains a list with functions for getting/setting the input value and the solution
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        ## Function for storing input values
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        ## Function for getting input values
        get <- function() x
        ## Function for saving the inversed matrix
        setsolve <- function(solve) s <<- solve
        ## Function for getting the inversed matrix
        getsolve <- function() s
        ## We return the list with the functions
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Trying to get solution from cache
        s <- x$getsolve()
        
        ## Not null means the solution is available from the cache,
        ## a message is output and the value is returned
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        
        ## If the solution is not available we get the input value
        data <- x$get()
        
        ## Inversing the matrix
        s <- solve(data, ...)
        
        ## Saving the result in the cache
        x$setsolve(s)
        s
}

## Test section

## Function for the creation of an invertible matrix (from the solve help)
hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }

## Creation of an invertible matrix
h8 <- hilbert(8)
## Create cache "matrix"
cache8 <- makeCacheMatrix(h8)
## Try cacheSolve, no message during the first run
sh8 <- cacheSolve(cache8)
## Test if solution is correct
round(sh8 %*% h8, 3)

## Try again cacheSolve, "getting cached data" will appear
sh8 <- cacheSolve(cache8)


