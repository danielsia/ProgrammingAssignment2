
##This function creates a special "vector", which is a list containing a function to set the value of the vector, get the value of the vector, set the value of each element in the inverse matrix and gets the value of each element in the inverse matrix.

makeCacheMatrix <- function(x = matrix()){
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }##set the value of the vector
        get <- function() x ##gets the value of the vector
        setinverse <- function(solve) i <<- solve ##sets the value of each element in the inverse matrix
        getinverse <- function() i ##gets the value of each element in the inverse matrix
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##This function computes the inverse of the matrix, only if it has not already done so in an earlier function. If it has, it returns the inverse that has been computed.

cacheSolve <- function(x, ...){
        i <- x$getinverse()
        if (!is.null(i)){ ##if the inverse is already computed before, it returns the computed inverse
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
## Returns a matrix that is the inverse of 'x'
