## R Programmign assigment 2

## This assigment will implement 2 functions to cache time-consuming operations.
## In this case, the time-consuming operation we will cache is getting the inverse of a matrix.
## Therefore, we will implement 2 functions: 
## 1- makeCacheMatrix, that will handle the cache of the inverse matrix
## 2- cacheSolve that will return the invere of a matrix. If it was already cached it will use the cache, 
## otherwise, will calculate and cache it

## #################
## Example of usage:
## #################


## > source('cachematrix.R')
## > m<-makeCacheMatrix(matrix(c(1.00,-0.25,-0.25,1.00) ,2,2))
## > m$get()
## [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00
## > cacheSolve(m)
## [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > cacheSolve(m)
## Inverse was calculated. We don't need to calculate it
          [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > 
##


## #############################
## The first function, makeCacheMatrix creates a special "vector", which is really a list containing 4 functions oto operate with the cache:
## 1- a function to set the matrix
## 2- a function to get the matrix
## 3- a function to set the matrix inverse
## 4- a function to get the matrix inverse
## this way, all 4 operations we need will be grouped into this vector
## #############################
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    ## set funcito- On set we set the matrix, and also clear the inverse if it ws laready cached.
    ##This is, if ithe inverse was already calculated, it is no longer valid, and we need to nullify it
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setinversematrix <- function(matrix) m <<- matrix
    
    getinversematrix <- function() m
    
    #build the vector with the 4 operations
    list(set = set, 
         get = get,
         setinversematrix = setinversematrix,
         getinversematrix = getinversematrix)
}

## #############################
## Returns teh inverse of the matrix created with makeCacheMatrix
## If the inverse was already calculated, it will use form the 'cache'
## If it was nto already calculated, ti will calcualte it, cache it and return it
## #############################
cacheSolve <- function(x, ...) {
    
    m <- x$getinversematrix()
    if(!is.null(m)) {
        message("Inverse was calculated. We don't need to calculate it")
        return(m)
    }
    ## we need to calculate the inverse
    data <- x$get()
    
    ##calculate the inverse
    i <- solve(data)
    ## set it for nest usages (in cache)
    x$setinversematrix(i)
    ## return it
    i
}
