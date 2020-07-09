rm(list = ls())


makeCacheMatrix <- function(x = matrix()) {
        inversion <- NULL
        set <- function(y) {
                x <<- y
                inversion <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inversion <<- inverse
        getInverse <- function() inversion
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        mat <- x$get()
        m <- solve(mat,...)
        x$setInverse(m)
        m
}

