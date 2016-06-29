## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv_mx <- NULL
        set <-function(y){
                x <<- y
                inv_mx <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv_mx <<- solve
        getinv <- function() inv_mx
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from the 
## cache.

cacheSolve <- function(x, ...) {
        inv_mx <- x$getinv()
        if(!is.null(inv_mx)){
                message("getting cached data")
                return(inv_mx)
        }
        data<- x$get()
        inv_mx <-solve(data,...)
        x$setinv(inv_mx)
        inv_mx
        ## Return a matrix that is the inverse of 'x'
}
