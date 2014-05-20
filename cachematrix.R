## Functions created for the Coursera R Programming Class
## Assignment 2
## Create a mechanism to cache a matrix's inverse

## Create a special "inverse matrix vector" which provides accessors and mutators
## to store the matrix (get/set) and its inverse (getinverse/setinverse).  Note
## that we delay computing the inverse of the matrix until the first time we ask
## for it.  We also clear the computed inverse when we re-set() the matrix value.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(x) {
        x <<- x
        inverse <<- NULL
    }
    get <- function() {
        return(x)
    }
    getinverse <- function() {
        if (is.null(inverse)) {
            message("compute and store inverse")
            inverse <<- solve(x)
        }
        message("return inverse from cache")
        return(inverse)
    }
    return( list(set=set, get=get, getinverse=getinverse) )
}

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    i <- x$getinverse(x)
    return(i)
}
