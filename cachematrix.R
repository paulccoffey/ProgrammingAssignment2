## A function to create a special matrix that can cache it's inverse and then
## compute the inverse of the special "matrix" returned.

## This first "makeCacheMatrix" section creates the matrix which we assume to be invertible

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                     ## set initial inverse value to NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}           
        setinverse <- function(inverse) {inv <<- inverse}
        getinverse <- function() {inv}
        list(set = set,                 ## gives the name 'set' to the set() function defined above
             get = get,                 ## gives the name 'get' to the get() function defined above
             setinverse = setinverse,   ## gives the name 'setinverse' to the setinverse() function defined above
             getinverse = getinverse)   ## gives the name 'getinverse' to the getinverse() function defined above
}

## This second "cacheSolve" section computes the inverse of the previously created matrix

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {             # check to see if the inverse value is NULL
                message("getting cached inverse")   
                return(inv)                   # return inverse value
        }
        dat <- x$get()
        inv <- solve(dat, ...)          # Use the solve function to get the inverse
        x$setinverse(inv)       
        inv
}
