## "R-programming" Coursera, Programming Assignment 2: Lexical Scoping 2016-07-10
##  David Lennox-Hvenekilde

## The two following functions 'makeCacheMatrix' and 'CacheSolve'
## are together able to solve and cache the inverse of a square, invertible matrix
## so that if the inverse has been calculated and has not changed, it is fetched 
## from the cache instead of begin calculated again.


## makeCacheMatrix() takes a matrix creates a list of functions that:
## 1. Sets the value of the matrix
## 2. Gets the value of the matrix
## 3. Sets the value of the inverse of the matrix
## 4. Gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve() computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}

## =========================================================================
## Example of using the above functions:
## A 10x10 matrix with random uniform distribution
A <- matrix(runif(10*10, min=-10, max=10), nrow=10,ncol=10) 
Acache<-makeCacheMatrix(A)
##IF you run cachesolve (below), the first time it will calculate the inverse,
## The subsequent time it will retrieve the inverse from the cache.
cacheSolve(Acache)
