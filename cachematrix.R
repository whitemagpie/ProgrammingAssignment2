## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    matInv <- NULL
    ## this function sets the matrix
    set <- function(y) {
        x <<- y
        matInv <<- NULL
    }
    ## this function returns the matrix
    get <- function() x
    
    ## this function sets the inverse of matrix in cache
    setInv <- function(inv) matInv <<- inv
    
    ## this function gets the inverse set above and returns to the caller
    getInv <- function() matInv
    
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
    
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    ## get the matrix and its inverse stored in the cache
    y <- x$get()
    matInv <- x$getInv()
    
    ## check if the stored matrix is identical to the one passed as arguement to cacheSolve
    ## If identical and if inverse has already been cached (inverse is not null), 
    ## then return the cached value
    if(identical(x,y) & !is.null(matInv)) {
        message("getting cached data")
        return(matInv)
    }
    
    
    ## if matrix has changed, then store new matrix in cache.
    if (!identical(x,y)) {
        x$set(x)
    }
    
    ## then calculate and store inverse in cache and return the inverse
    matInv <- solve(x, ...)
    x$setInv(matInv)    
    matInv
}
