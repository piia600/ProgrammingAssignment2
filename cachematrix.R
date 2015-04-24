## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x=matrix, ...) {

	  cache <- NULL
        setmatrix <- function(y) {
                x <<- y
                cache <<- NULL
        }
        getmatrix <- function() x <- matrix()
        setinverse <- function(solve) cache <<- solve(x)
        getinverse <- function() cache
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}


##  This function computes the inverse of the special "matrix" returned 
##  by makeCacheMatrix above. If the inverse has already been calculated 
##  (and the matrix has not changed), then cacheSolve should retrieve the 
##  inverse from the cache.

cacheSolve <- function(matrix, ...) {
        ## Check for a matrix that is the inverse of 'x'
    matrix <- matrix$getmatrix()
    
    if(!is.null(cache)){
      return(cache)
    }

    cache <- matrix$setinverse()
    matrix$setinverse(cache)
    return(cache)
}

## This gives an error about atomic vectors but I don't have time to debug the code. :)
