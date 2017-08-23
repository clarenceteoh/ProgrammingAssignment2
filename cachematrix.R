## The two functions in this script are used for calculating the inverse
## of a matrix. These functions allow the inverse of a matrix to be 
## calculated once in the initial call. For subsequent calls, the value 
## of the inverse which has already been stored is returned. This reduces 
## a lot of computational cost and time.

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    INV <- NULL
    set <- function(y) {
      x <<- y
      INV <<- NULL
    }
    get <- function() x
    set_inverse <- function(solve) INV <<- solve
    get_inverse <- function() INV
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## cacheSolve: This function computes the inverse 
## of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix 
## has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    INV <- x$get_inverse()
    if(!is.null(INV)) {
      message("getting cached data")
      return(INV)
    }
        
    data <- x$get()
    INV <- solve(data, ...)
    x$set_inverse(INV)
    INV
}
