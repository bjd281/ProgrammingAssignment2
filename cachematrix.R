## Put comments here that give an overall description of what your
## functions do 

## example usage of makeCacheMatrix and cachesolve
## the usage below shows that x is assigned a "matrix" via makeCacheMatrix,
## and the inverse matrix is calculated from cachesolve.
## the 2nd run of cachesolve shows that the inverse is obtained from the cache since x has not changed.
## if x is reassigned (even with the same matrix), cachesolve will recalculate the inverse
## 
## x<-makeCacheMatrix(c(4,3,3,2),2,2)
## Error in makeCacheMatrix(c(4, 3, 3, 2), 2, 2) : unused arguments (2, 2)
## > x<-makeCacheMatrix(matrix(c(4,3,3,2),2,2))
## > cachesolve(x)
##      [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4
## > cachesolve(x)
## getting cached data
##      [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4



## -----------------------------------------------------------------------------------------------------------------
## Write a short comment describing this function
## makeCacheMatrix creates a "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
m <- NULL
set <- function(y) {
x <<- y
m <<- NULL
}
get <- function() x
setsolve <- function(solve) m <<- solve
getsolve <- function() m
list(set = set, get = get,
setsolve = setsolve,
getsolve = getsolve)
}



## -----------------------------------------------------------------------------------------------------------------
## Write a short comment describing this function
## cacheSolve calculates the inverse of the matrix of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the
## inverse in the cache via the setsolve function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
m <- x$getsolve()
if(!is.null(m)) {
message("getting cached data")
return(m)
}
data <- x$get()
m <- solve(data, ...)
x$setsolve(m)
m
}
