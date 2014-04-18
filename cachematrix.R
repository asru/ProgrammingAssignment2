########################################################################
##
## Purpose: create an R MATRIX object that will cache
##          its INVERSE to reduce computational overhead.
##
## Usage  : 1. call 'makeCacheMatrix(<matrix_to_invert>)'
##          2. call 'cacheSolve(<cache_matrix>)' when you
##             need the inverse of the matrix.
##          Future calls to 'cacheSolve()' with a previously
##          solved matrix will return the cached inverse.
##          You will also see a message that 'cached data' is used.
##
## Notes  : Minimal (read NO) ERROR CHECKING in these functions!
##          Assume <matrix_to_invert> is SQUARE and INVERTIBLE!
##
##          ERRORS will be raised from 'solve()' if <matrix_to_invert>
##          is either not square or singular (cannot be inverted).
##
## Author : asru -- with skeleton file and instructions forked from
##
##          https://github.com/rdpeng/ProgrammingAssignment2
##
########################################################################

########################################################################
##
## Function that creates a special R MATRIX object with functions to:
##
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
## 
## NOTE: to get the benefit of this matrix object, computation of the 
##       inverse has to be via a call to the 'cacheSolve()' function below.
##

makeCacheMatrix <- function(x = matrix()) {
    ## initialize cached matrix to NULL
    m <- NULL
    
    ## create the matrix object
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## return the initial matrix -- used in 'cacheSolve()' below
    get <- function() x
    
    ## use solve() to compute the inverse of the initial matrix,
    ## then assign the solution to our cached matrix m
    setinverse <- function(solve) m <<- solve
    
    ## return the cached matrix -- used in 'cacheSolve()' below
    getinverse <- function() m
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

########################################################################
## 
##  Function that returns the inverse of matrix objects created by 
##  'makeCacheMatrix()' above.
##
##  If a cached inverse is returned, the function writes out a message.
##

cacheSolve <- function(x, ...) {
    ## First check if the inverse matrix is cached.
    ## If so, write a message, then return the cached inverse.
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## Matrix does NOT have a cached inverse, so compute
    ## inverse, assign it to the cache, then return the
    ## computed inverse.
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

########################################################################
##
##  EXAMPLE USAGE:
##  --------------
##  > myMatrix <- makeCacheMatrix(matrix(1:4,2,2))
##  > myMatrix$get() # INITIAL matrix
##  [,1] [,2]
##  [1,]    1    3
##  [2,]    2    4
##  > cacheSolve(myMatrix) # FIRST call to cacheSolve()
##  [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5
##  > cacheSolve(myMatrix) # SECOND call to cacheSolve()
##  getting cached data    # NOTE message to log - cached data used 
##  [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5
##  > 
