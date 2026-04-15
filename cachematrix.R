## this function stores a matrix and also saves the inverse so it doesnt
## have to keep recalculating it every single time.

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL  ## starts as null bc we havent calculated anything yet
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() {
        return(x)  ## just returns the matrix
    }
    
    setInverse <- function(inverse) {
        inv <<- inverse
    }
    
    getInverse <- function() {
        return(inv)
    }
    
## putting everything in a list so we can access it later

    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## this one actually gets the inverse but it first checks if we already
## calculated it theres no extra work

cacheSolve <- function(x, ...) {
    
    inv <- x$getInverse()
    
    ## if its not null that means we already have it saved
    if(!is.null(inv)) {
        message("already have this one, grabbing from cache")
        return(inv)
    }
    
    ## otherwise we have to actually calculate it
    mat <- x$get()
    inv <- solve(mat)  ## solve() gets the inverse
    
    x$setInverse(inv)  ## save it so next time we dont have to redo this
    
    return(inv)  ## Return a matrix that is the inverse of 'x'
}
