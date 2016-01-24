## Put comments here that give an overall description of what your
## functions do

## A function that create a special "matrix"

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse 
        getinverse <- function() i        
        list(set = set, 
               get = get, 
               setinverse = setinverse, 
               getinverse = getinverse)
}


## This function return the inverse of a matrix, if inverse cached, return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached matrix")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
