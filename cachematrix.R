## Functions created for the Coursera R Programming Class
## Assignment 2
## Create a mechanism to cache a matrix's inverse

## Create a special "inverse matrix vector" which provides accessors and mutators
## to store the matrix (get/set) and its inverse (getinverse/setinverse).  Note
## that we delay computing the inverse of the matrix until the first time we ask
## for it.  We also clear the computed inverse when we re-set() the matrix value.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    ## The set() method provides a way to change the matrix after creating
    ## this special "cacheMatrix" object.  Note that when we change the
    ## matrix we clear any cached inverse we may have previously computed.
    set <- function(x) {
        x <<- x
        inverse <<- NULL
    }
    ## The get() method returns the matrix value from the "cacheMatrix" object.
    get <- function() {
        return(x)
    }
    ## The getinverse() method returns the inverse of the matrix in our object.
    ## This is the method that really does the work.  We detect when we have
    ## not yet computed the inverse, compute it with the solve() method, and
    ## store it in the object.  From then on, as long as the matrix itself
    ## is not changed (with the set() method) we will return this cached value.
    ## (I left the debugging messages in so you can see what's happening.)
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

## Return a matrix that is the inverse of our special "cacheMatrix" object "x"
cacheSolve <- function(x, ...) {
    ## Our special matrix vector x contains a method for getting the inverse
    ## We know that this inverse will be the inverse of the current matrix
    ## value, because we arrange to store both the matrix and its inverse.
    ## We compute the inverse when we first request it (lazy instantiation).
    i <- x$getinverse(x)
    return(i)
}
