
#--------------------------------------------------------------------------------------------
#Assignment: Caching the inverse of a Matrix
##Write the following functions:

#1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#2cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##            If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
##            should retrieve the inverse from the cache

source("assessment3.R")
makeCacheMatrix <- function(x = matrix()) {
        n <- NULL
        set <- function(y) {                                                          # store a matrix
                x <<- y
                n <<- NULL                                                        # since the matrix is assigned a new value, flush the cache
        }
        get <- function() x                                                           # returns the stored matrix
        setinverse <- function(solve) n <<- solve                                 # cache the given argument 
        getinverse <- function() n                                                # get the cached value
        list(set= set, get = get, setinverse = setinverse, getinverse = getinverse)   # return a list. Each named element of the list is a function
}


# The following function calculates the inverse of a "special" matrix created with 
# makeCacheMatrix
cacheSolve <- function(x, ...) {
       n <- x$getinverse()                              # get the cached value
        if(!is.null(n)) {                               # if a cached value exists return it
                message("getting cached data")
                return(n)
        }
        data <- x$get()                                 # otherwise get the matrix, caclulate the inverse and store it in
           n <- solve(data,...)                        # the cache
        x$setinverse(n)
        n                                               # return the inverse
}
##Examples for tetsing the functions
#------------------------------------
#Example 1
e<-diag(7,7)  #creating a simple matrix with the value 7 in the diagonal
e

cache_matrix <- makeCacheMatrix(e)          
cacheSolve(cache_matrix)   #cached data


#Example 2
e2<-matrix(c(1,4,7,2), 2, 2)  #creating a 2x2 matrix
e2

cache_matrix2<-makeCacheMatrix(e2) 
cacheSolve(cache_matrix2)

#Example 3
e3<-makeCacheMatrix(matrix(6:9,2,2))        #creating a matrix within the function
e3$get()
e3$getinverse()                             #here the result is NULL 
cacheSolve(e3)                              #getting the cached data
e3$getinverse()                              #after using the function cacheSolve, we can see the values of getinverse

