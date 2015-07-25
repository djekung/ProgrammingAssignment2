## The function initializes an inverted matrix (i-matrix) which is cached in
## an other environment. It's allow to get that i-matrix with the function 'solve'
## Then we have here a cached i-matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function (y = matrix){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function allow us to call the inverted matrix when it was already
## computeded by the function above otherwise it computes the inverse of that
## matrix

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data,...)
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
}
