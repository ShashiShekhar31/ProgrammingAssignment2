## The function has two parts makeCacheMatrix() and cacheSolve whcih is used to 
## take a matrix as input and return inverse as well as save the result the 
## value in cache so that next time it does not have to calculate the inverse 
## if the input matrix is same

## makeCacheMatrix is used to get and set the value of matrix as well as get and
## set the value of inverse of the matric

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function checks if the inverse of the matrix is already calculated
## if so it returns the same else we calculate the inverse of the matrix using
## solve function, sets the cache and return the inverse of the matrix

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        ## check if the inverse already exists
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
                
        }
        ## if inverse doesnot exist in the cache, calculate the inverse and 
        ## return the same
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
