# Assignment: Caching the inverse of a matrix
# Course: R programming
# Author: Dorien Huijser
# Date: 20191231
# Assumptions: matrix supplied is always invertible


## Function 1: makeCacheMatrix ####
## makeCacheMatrix creates a special matrix object that can cache its 
## inverse (inv)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y # store input matrix (y) as x in parent environment
        inv <<- NULL # inv as NULL in parent environment, clears previous values of inv
    }
    get <- function() x # get matrix x (from parent environment)
    setsolve <- function(solve) inv <<- solve(x) # setter for the inverse, inv needs to be accessed after setsolve() completes <<- 
    getsolve <- function() inv # get the inverse inv
    list(set = set, get=get, setsolve = setsolve, getsolve=getsolve) # named list of functions returned to parent environment
}

## Function 2: cacheSolve ####
## cacheSolve computes the inverse of the special matrix returned by 
## makeCacheMatrix above. If the inverse has already been calculated,
## (and the matrix has not changed), the cachesolve retrieves the 
## inverse from the cache

cacheSolve <- function(x, ...) {
    inv <- x$getsolve() #retrieve an inverse from the input x
    if(!is.null(inv)){ #if value is not equal to 0, there is a cached mean that can be returned
        message("getting cached data")
        return(inv)
    }
    data <- x$get() #if there is no inverse, get the vector, ...
    inv <- solve(data,...) # calculate the inverse ...
    x$setsolve(inv) # set the inverse in the input object and...
    inv # return the value to the parent environment
}




##############################################################
### Not important for this assignment ###
## Example caching mean of a vector - for myself ####
# <<- assigns a value to an object in a differen environment than the current
# makeVector: set vector value, get vector value, set mean value, get mean value
makeVector <- function(x=numeric()) { #output contains complete copy of the environment within this function
    m <- NULL
    set <- function(y) {
        x <<- y # store input (y) as x in parent environment
        m <<- NULL # m as NULL in parent environment, clears previous values of m
    }
    get <- function() x # define getter for vector x (from parent environment)
    setmean <- function(mean) m <<- mean # setter for the mean, m needs to be accessed after setmean() completes <<- 
    getmean <- function() m # getter for the mean m
    list(set = set, get=get, setmean = setmean, getmean=getmean) # named list of functions returned to parent environment
}

# cacheMean: calculate mean of special vector, checks if already calculated
# requires input argument of type makeVector()
cacheMean <- function(x,...) {
    m <- x$getmean() #retrieve a mean from the input x
    if(!is.null(m)){ #if value is not equal to 0, there is a cached mean that can be returned
        message("getting cached data")
        return(m)
    }
    data <- x$get() #if there is no mean, get the vector, ...
    m <- mean(data,...) # calculate the mean ...
    x$setmean(m) # set the mean in the input object and...
    m # return the value to the parent environment
}
