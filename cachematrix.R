## OVERVIEW:
## These 2 functions, makeCasheMatrix and cacheSolve work together
## to create a special object that stores a matrix, calculates & 
## caches its inverse.
##
## ASSUMPTION:  matrix stored is invertible 
##
## makeCasheMatrix
##      creates a list of functions which operate on the matrix
##      the functions are: set, get, setinverse & getinverse
##
## cacheSolve
##      at first entry, uses solve() to calculate inverse of matrix
##      stores the inverse, via setinverse.
##      subsequent entries, simply retrieve the stored inverse
##



makeCacheMatrix <- function(x = matrix())  {
        
## creates a special "matrix" object that can cache its inverse
## through a list of functions; set, get, setinverse & getinverse
## these operate upon the matrix and its inverse
##      1. set-sets the value of x to the matrix
##      2. get-retrieves the value of x
##      3. setinverse-sets the value of invM to the inverse of matrix x
##      4. getinverse-retrieves the value of the inverted matrix invM
        
        invM <- NULL
        set <- function(y) {
                x <<- y
                invM <<- NULL
        }
        get <- function() x
        setinverse <- function(invMatrix) invM <<- invMatrix
        getinverse <- function() invM
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
##
##      checks for stored value of inverse, via x$getinverse()
##              returns the inverse matrix if found
##      if not already cached, 
##              calculates the inverse, via solve()
##              caches the inverse, via x$setinverse()
        
        invM <- x$getinverse()
        if(!is.null(invM)) {
                message("getting cached data")
                return(invM)
        }
        m <- x$get()
        invM <- solve(m, ...)
        x$setinverse(invM)
        invM
}
